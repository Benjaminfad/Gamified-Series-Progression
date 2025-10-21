(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_EPISODE (err u101))
(define-constant ERR_VOTING_CLOSED (err u102))
(define-constant ERR_ALREADY_VOTED (err u103))
(define-constant ERR_INSUFFICIENT_TOKENS (err u104))
(define-constant ERR_EPISODE_NOT_FOUND (err u105))
(define-constant ERR_BRANCH_NOT_FOUND (err u106))
(define-constant ERR_VOTING_STILL_ACTIVE (err u107))
(define-constant ERR_INSUFFICIENT_REPUTATION (err u108))
(define-constant ERR_REWARD_ALREADY_CLAIMED (err u109))
(define-constant ERR_NO_WINNING_VOTE (err u110))
(define-constant ERR_ANALYTICS_NOT_FOUND (err u111))

(define-constant BASE_REPUTATION_REWARD u50)
(define-constant STREAK_BONUS_MULTIPLIER u25)
(define-constant MAX_VOTING_WEIGHT u300)
(define-constant REPUTATION_THRESHOLD_VIP u500)
(define-constant REPUTATION_THRESHOLD_ELITE u1000)

(define-data-var next-episode-id uint u1)
(define-data-var total-token-supply uint u1000000)
(define-data-var voting-duration uint u144)
(define-data-var total-reputation-distributed uint u0)
(define-data-var reward-pool-balance uint u100000)
(define-data-var total-votes-cast uint u0)
(define-data-var total-episodes-completed uint u0)
(define-data-var total-unique-voters uint u0)

(define-map token-balances principal uint)
(define-map episodes 
    uint 
    {
        title: (string-ascii 50),
        description: (string-ascii 200),
        voting-end: uint,
        is-active: bool,
        total-votes: uint,
        winner-branch: (optional uint)
    }
)

(define-map episode-branches 
    {episode-id: uint, branch-id: uint}
    {
        title: (string-ascii 50),
        description: (string-ascii 200),
        vote-count: uint
    }
)

(define-map user-votes 
    {voter: principal, episode-id: uint}
    {branch-id: uint, token-amount: uint}
)

(define-map episode-participants uint (list 100 principal))

(define-map user-reputation
    principal
    {
        total-reputation: uint,
        voting-streak: uint,
        total-votes: uint,
        correct-votes: uint,
        last-vote-episode: uint,
        privilege-level: uint
    }
)

(define-map episode-rewards
    {user: principal, episode-id: uint}
    {
        token-reward: uint,
        reputation-reward: uint,
        claimed: bool,
        voting-weight: uint
    }
)

(define-map user-privileges
    principal
    {
        voting-weight-multiplier: uint,
        early-access: bool,
        bonus-token-rate: uint
    }
)

(define-map episode-analytics
    uint
    {
        total-participants: uint,
        total-tokens-voted: uint,
        winning-margin: uint,
        consensus-strength: uint,
        average-vote-size: uint,
        completion-timestamp: uint
    }
)

(define-map branch-analytics
    {episode-id: uint, branch-id: uint}
    {
        vote-percentage: uint,
        unique-voters: uint,
        largest-single-vote: uint,
        average-vote-weight: uint
    }
)

(define-map user-voting-history
    principal
    {
        episodes-participated: uint,
        total-tokens-voted: uint,
        win-rate: uint,
        average-vote-size: uint,
        favorite-branch-type: uint,
        first-vote-episode: uint
    }
)

(define-map community-metrics
    uint
    {
        active-voters-count: uint,
        engagement-rate: uint,
        average-consensus: uint,
        total-activity: uint
    }
)

(define-public (transfer-tokens (amount uint) (recipient principal))
    (let ((sender-balance (default-to u0 (map-get? token-balances tx-sender))))
        (asserts! (>= sender-balance amount) ERR_INSUFFICIENT_TOKENS)
        (map-set token-balances tx-sender (- sender-balance amount))
        (map-set token-balances recipient 
            (+ amount (default-to u0 (map-get? token-balances recipient))))
        (ok true)
    )
)

(define-public (create-episode (title (string-ascii 50)) (description (string-ascii 200)))
    (let ((episode-id (var-get next-episode-id)))
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (map-set episodes episode-id
            {
                title: title,
                description: description,
                voting-end: (+ stacks-block-height (var-get voting-duration)),
                is-active: true,
                total-votes: u0,
                winner-branch: none
            }
        )
        (var-set next-episode-id (+ episode-id u1))
        (ok episode-id)
    )
)

(define-public (add-plot-branch 
    (episode-id uint) 
    (branch-id uint) 
    (title (string-ascii 50)) 
    (description (string-ascii 200))
)
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (is-some (map-get? episodes episode-id)) ERR_EPISODE_NOT_FOUND)
        (map-set episode-branches {episode-id: episode-id, branch-id: branch-id}
            {
                title: title,
                description: description,
                vote-count: u0
            }
        )
        (ok true)
    )
)

(define-public (vote-on-branch (episode-id uint) (branch-id uint) (token-amount uint))
    (let (
        (episode-info (unwrap! (map-get? episodes episode-id) ERR_EPISODE_NOT_FOUND))
        (voter-balance (default-to u0 (map-get? token-balances tx-sender)))
        (current-vote (map-get? user-votes {voter: tx-sender, episode-id: episode-id}))
        (branch-info (unwrap! (map-get? episode-branches {episode-id: episode-id, branch-id: branch-id}) ERR_BRANCH_NOT_FOUND))
    )
        (asserts! (get is-active episode-info) ERR_VOTING_CLOSED)
        (asserts! (<= stacks-block-height (get voting-end episode-info)) ERR_VOTING_CLOSED)
        (asserts! (>= voter-balance token-amount) ERR_INSUFFICIENT_TOKENS)
        (asserts! (is-none current-vote) ERR_ALREADY_VOTED)
        
        (map-set user-votes {voter: tx-sender, episode-id: episode-id}
            {branch-id: branch-id, token-amount: token-amount}
        )
        
        (map-set episode-branches {episode-id: episode-id, branch-id: branch-id}
            (merge branch-info {vote-count: (+ (get vote-count branch-info) token-amount)})
        )
        
        (map-set episodes episode-id
            (merge episode-info {total-votes: (+ (get total-votes episode-info) token-amount)})
        )
        
        (add-participant episode-id tx-sender)
        (unwrap-panic (update-voting-analytics tx-sender episode-id branch-id token-amount))
        (var-set total-votes-cast (+ (var-get total-votes-cast) u1))
        (ok true)
    )
)

(define-private (add-participant (episode-id uint) (participant principal))
    (let ((current-participants (default-to (list) (map-get? episode-participants episode-id))))
        (match (as-max-len? (append current-participants participant) u100)
            new-list (begin
                (map-set episode-participants episode-id new-list)
                true
            )
            (begin
                (map-set episode-participants episode-id (list participant))
                true
            )
        )
    )
)

(define-public (finalize-episode (episode-id uint))
    (let (
        (episode-info (unwrap! (map-get? episodes episode-id) ERR_EPISODE_NOT_FOUND))
        (winning-branch (find-winning-branch episode-id))
    )
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (> stacks-block-height (get voting-end episode-info)) ERR_VOTING_STILL_ACTIVE)
        
        (map-set episodes episode-id
            (merge episode-info {
                is-active: false,
                winner-branch: winning-branch
            })
        )
        
        (try! (batch-calculate-rewards episode-id))
        (try! (finalize-episode-analytics episode-id))
        (var-set total-episodes-completed (+ (var-get total-episodes-completed) u1))
        (ok winning-branch)
    )
)

(define-private (find-winning-branch (episode-id uint))
    (let (
        (branch-1 (map-get? episode-branches {episode-id: episode-id, branch-id: u1}))
        (branch-2 (map-get? episode-branches {episode-id: episode-id, branch-id: u2}))
        (branch-3 (map-get? episode-branches {episode-id: episode-id, branch-id: u3}))
    )
        (if (and (is-some branch-1) (is-some branch-2))
            (if (>= (get vote-count (unwrap-panic branch-1)) (get vote-count (unwrap-panic branch-2)))
                (if (and (is-some branch-3) (>= (get vote-count (unwrap-panic branch-3)) (get vote-count (unwrap-panic branch-1))))
                    (some u3)
                    (some u1)
                )
                (if (and (is-some branch-3) (>= (get vote-count (unwrap-panic branch-3)) (get vote-count (unwrap-panic branch-2))))
                    (some u3)
                    (some u2)
                )
            )
            (if (is-some branch-1) (some u1) none)
        )
    )
)

(define-public (distribute-single-token (recipient principal) (amount uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (map-set token-balances recipient
            (+ amount (default-to u0 (map-get? token-balances recipient)))
        )
        (ok true)
    )
)

(define-public (set-voting-duration (blocks uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (var-set voting-duration blocks)
        (ok true)
    )
)

(define-private (get-user-reputation-data (user principal))
    (default-to
        {
            total-reputation: u0,
            voting-streak: u0,
            total-votes: u0,
            correct-votes: u0,
            last-vote-episode: u0,
            privilege-level: u0
        }
        (map-get? user-reputation user)
    )
)

(define-private (calculate-reputation-reward (user principal) (episode-id uint) (correct-vote bool))
    (let (
        (user-rep (get-user-reputation-data user))
        (streak-bonus (if (and (> (get voting-streak user-rep) u0) correct-vote)
                        (* (get voting-streak user-rep) STREAK_BONUS_MULTIPLIER)
                        u0))
        (base-reward (if correct-vote BASE_REPUTATION_REWARD u0))
    )
        (+ base-reward streak-bonus)
    )
)

(define-private (update-user-reputation (user principal) (episode-id uint) (correct-vote bool))
    (let (
        (user-rep (get-user-reputation-data user))
        (new-streak (if correct-vote
                      (if (is-eq (+ (get last-vote-episode user-rep) u1) episode-id)
                        (+ (get voting-streak user-rep) u1)
                        u1)
                      u0))
        (reputation-reward (calculate-reputation-reward user episode-id correct-vote))
        (new-total-rep (+ (get total-reputation user-rep) reputation-reward))
        (new-privilege-level (if (>= new-total-rep REPUTATION_THRESHOLD_ELITE) u2
                               (if (>= new-total-rep REPUTATION_THRESHOLD_VIP) u1 u0)))
    )
        (map-set user-reputation user
            {
                total-reputation: new-total-rep,
                voting-streak: new-streak,
                total-votes: (+ (get total-votes user-rep) u1),
                correct-votes: (if correct-vote (+ (get correct-votes user-rep) u1) (get correct-votes user-rep)),
                last-vote-episode: episode-id,
                privilege-level: new-privilege-level
            }
        )
        (update-user-privileges user new-privilege-level)
        reputation-reward
    )
)

(define-private (update-user-privileges (user principal) (privilege-level uint))
    (let (
        (weight-multiplier (if (is-eq privilege-level u2) u200
                            (if (is-eq privilege-level u1) u150 u100)))
        (early-access (>= privilege-level u1))
        (bonus-rate (if (is-eq privilege-level u2) u10
                      (if (is-eq privilege-level u1) u5 u0)))
    )
        (map-set user-privileges user
            {
                voting-weight-multiplier: weight-multiplier,
                early-access: early-access,
                bonus-token-rate: bonus-rate
            }
        )
    )
)

(define-public (calculate-episode-rewards (episode-id uint) (user principal))
    (let (
        (episode-info (unwrap! (map-get? episodes episode-id) ERR_EPISODE_NOT_FOUND))
        (user-vote (unwrap! (map-get? user-votes {voter: user, episode-id: episode-id}) ERR_NO_WINNING_VOTE))
        (winning-branch (unwrap! (get winner-branch episode-info) ERR_NO_WINNING_VOTE))
        (correct-vote (is-eq (get branch-id user-vote) winning-branch))
        (user-privilege-data (default-to {voting-weight-multiplier: u100, early-access: false, bonus-token-rate: u0} 
                           (map-get? user-privileges user)))
        (base-token-reward (if correct-vote (get token-amount user-vote) u0))
        (bonus-multiplier (/ (get bonus-token-rate user-privilege-data) u10))
        (token-reward (+ base-token-reward (* base-token-reward bonus-multiplier)))
        (reputation-reward (update-user-reputation user episode-id correct-vote))
        (voting-weight (/ (* (get token-amount user-vote) (get voting-weight-multiplier user-privilege-data)) u100))
    )
        (asserts! (not (get is-active episode-info)) ERR_VOTING_STILL_ACTIVE)
        (asserts! (is-none (map-get? episode-rewards {user: user, episode-id: episode-id})) ERR_REWARD_ALREADY_CLAIMED)
        
        (map-set episode-rewards {user: user, episode-id: episode-id}
            {
                token-reward: token-reward,
                reputation-reward: reputation-reward,
                claimed: false,
                voting-weight: voting-weight
            }
        )
        (ok {
            token-reward: token-reward,
            reputation-reward: reputation-reward,
            correct-vote: correct-vote
        })
    )
)

(define-public (claim-episode-reward (episode-id uint))
    (let (
        (reward-data (unwrap! (map-get? episode-rewards {user: tx-sender, episode-id: episode-id}) ERR_NO_WINNING_VOTE))
        (current-balance (default-to u0 (map-get? token-balances tx-sender)))
    )
        (asserts! (not (get claimed reward-data)) ERR_REWARD_ALREADY_CLAIMED)
        (asserts! (>= (var-get reward-pool-balance) (get token-reward reward-data)) ERR_INSUFFICIENT_TOKENS)
        
        (map-set token-balances tx-sender (+ current-balance (get token-reward reward-data)))
        (var-set reward-pool-balance (- (var-get reward-pool-balance) (get token-reward reward-data)))
        (var-set total-reputation-distributed (+ (var-get total-reputation-distributed) (get reputation-reward reward-data)))
        
        (map-set episode-rewards {user: tx-sender, episode-id: episode-id}
            (merge reward-data {claimed: true})
        )
        
        (ok {
            tokens-claimed: (get token-reward reward-data),
            reputation-earned: (get reputation-reward reward-data)
        })
    )
)

(define-public (batch-calculate-rewards (episode-id uint))
    (let (
        (episode-info (unwrap! (map-get? episodes episode-id) ERR_EPISODE_NOT_FOUND))
        (participants (get-episode-participants episode-id))
    )
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (not (get is-active episode-info)) ERR_VOTING_STILL_ACTIVE)
        
        (ok (map process-participant-reward participants))
    )
)

(define-private (process-participant-reward (user principal))
    (let (
        (current-episode (- (var-get next-episode-id) u1))
    )
        (match (calculate-episode-rewards current-episode user)
            success true
            error false
        )
    )
)

(define-public (generate-privilege-bonus (user principal))
    (let (
        (user-rep (get-user-reputation-data user))
        (privilege-data (default-to {voting-weight-multiplier: u100, early-access: false, bonus-token-rate: u0}
                      (map-get? user-privileges user)))
        (bonus-tokens (get bonus-token-rate privilege-data))
    )
        (asserts! (>= (get privilege-level user-rep) u1) ERR_INSUFFICIENT_REPUTATION)
        (asserts! (> bonus-tokens u0) ERR_INSUFFICIENT_REPUTATION)
        (asserts! (>= (var-get reward-pool-balance) bonus-tokens) ERR_INSUFFICIENT_TOKENS)
        
        (map-set token-balances user (+ bonus-tokens (default-to u0 (map-get? token-balances user))))
        (var-set reward-pool-balance (- (var-get reward-pool-balance) bonus-tokens))
        
        (ok bonus-tokens)
    )
)

(define-public (apply-voting-weight (episode-id uint) (user principal) (base-tokens uint))
    (let (
        (privilege-data (default-to {voting-weight-multiplier: u100, early-access: false, bonus-token-rate: u0}
                      (map-get? user-privileges user)))
        (weighted-tokens (/ (* base-tokens (get voting-weight-multiplier privilege-data)) u100))
        (capped-tokens (if (> weighted-tokens MAX_VOTING_WEIGHT) MAX_VOTING_WEIGHT weighted-tokens))
    )
        (ok capped-tokens)
    )
)

(define-public (grant-early-access (episode-id uint) (user principal))
    (let (
        (episode-info (unwrap! (map-get? episodes episode-id) ERR_EPISODE_NOT_FOUND))
        (privilege-data (default-to {voting-weight-multiplier: u100, early-access: false, bonus-token-rate: u0}
                      (map-get? user-privileges user)))
    )
        (asserts! (get early-access privilege-data) ERR_INSUFFICIENT_REPUTATION)
        (asserts! (get is-active episode-info) ERR_VOTING_CLOSED)
        
        (ok true)
    )
)

(define-read-only (get-token-balance (user principal))
    (default-to u0 (map-get? token-balances user))
)

(define-read-only (get-episode-info (episode-id uint))
    (map-get? episodes episode-id)
)

(define-read-only (get-branch-info (episode-id uint) (branch-id uint))
    (map-get? episode-branches {episode-id: episode-id, branch-id: branch-id})
)

(define-read-only (get-user-vote (voter principal) (episode-id uint))
    (map-get? user-votes {voter: voter, episode-id: episode-id})
)

(define-read-only (get-episode-participants (episode-id uint))
    (default-to (list) (map-get? episode-participants episode-id))
)

(define-read-only (get-current-episode-id)
    (- (var-get next-episode-id) u1)
)

(define-read-only (get-voting-duration)
    (var-get voting-duration)
)

(define-read-only (is-voting-active (episode-id uint))
    (match (map-get? episodes episode-id)
        episode-info (and 
            (get is-active episode-info) 
            (<= stacks-block-height (get voting-end episode-info))
        )
        false
    )
)

(define-read-only (get-user-reputation (user principal))
    (get-user-reputation-data user)
)

(define-read-only (get-user-privileges (user principal))
    (default-to {voting-weight-multiplier: u100, early-access: false, bonus-token-rate: u0}
      (map-get? user-privileges user))
)

(define-read-only (get-episode-reward (user principal) (episode-id uint))
    (map-get? episode-rewards {user: user, episode-id: episode-id})
)

(define-read-only (get-voting-accuracy (user principal))
    (let ((user-rep (get-user-reputation-data user)))
        (if (> (get total-votes user-rep) u0)
            (/ (* (get correct-votes user-rep) u100) (get total-votes user-rep))
            u0
        )
    )
)

(define-read-only (get-reputation-leaderboard)
    (ok {
        total-reputation-distributed: (var-get total-reputation-distributed),
        reward-pool-balance: (var-get reward-pool-balance)
    })
)

(define-read-only (get-user-privilege-level (user principal))
    (get privilege-level (get-user-reputation-data user))
)

(define-read-only (has-early-access (user principal))
    (get early-access (default-to {voting-weight-multiplier: u100, early-access: false, bonus-token-rate: u0}
                        (map-get? user-privileges user)))
)

(define-private (update-voting-analytics (voter principal) (episode-id uint) (branch-id uint) (token-amount uint))
    (let (
        (user-history (default-to 
            {episodes-participated: u0, total-tokens-voted: u0, win-rate: u0, average-vote-size: u0, favorite-branch-type: u0, first-vote-episode: u0}
            (map-get? user-voting-history voter)))
        (is-first-vote (is-eq (get episodes-participated user-history) u0))
    )
        (map-set user-voting-history voter
            {
                episodes-participated: (+ (get episodes-participated user-history) u1),
                total-tokens-voted: (+ (get total-tokens-voted user-history) token-amount),
                win-rate: (get win-rate user-history),
                average-vote-size: (/ (+ (get total-tokens-voted user-history) token-amount) (+ (get episodes-participated user-history) u1)),
                favorite-branch-type: branch-id,
                first-vote-episode: (if is-first-vote episode-id (get first-vote-episode user-history))
            })
        (ok true)
    )
)

(define-private (finalize-episode-analytics (episode-id uint))
    (let (
        (episode-info (unwrap! (map-get? episodes episode-id) ERR_EPISODE_NOT_FOUND))
        (participants (get-episode-participants episode-id))
        (participant-count (len participants))
        (total-tokens (get total-votes episode-info))
        (avg-vote (if (> participant-count u0) (/ total-tokens participant-count) u0))
        (winner (unwrap! (get winner-branch episode-info) ERR_NO_WINNING_VOTE))
        (winner-votes (get vote-count (unwrap! (map-get? episode-branches {episode-id: episode-id, branch-id: winner}) ERR_BRANCH_NOT_FOUND)))
        (consensus (if (> total-tokens u0) (/ (* winner-votes u100) total-tokens) u0))
    )
        (map-set episode-analytics episode-id
            {
                total-participants: participant-count,
                total-tokens-voted: total-tokens,
                winning-margin: winner-votes,
                consensus-strength: consensus,
                average-vote-size: avg-vote,
                completion-timestamp: stacks-block-height
            })
        
        (try! (calculate-branch-analytics episode-id u1))
        (try! (calculate-branch-analytics episode-id u2))
        (try! (calculate-branch-analytics episode-id u3))
        (try! (update-community-metrics episode-id))
        (ok true)
    )
)

(define-private (calculate-branch-analytics (episode-id uint) (branch-id uint))
    (let (
        (branch-info (map-get? episode-branches {episode-id: episode-id, branch-id: branch-id}))
        (episode-info (unwrap! (map-get? episodes episode-id) ERR_EPISODE_NOT_FOUND))
    )
        (match branch-info
            branch-data
            (let (
                (total-votes (get total-votes episode-info))
                (branch-votes (get vote-count branch-data))
                (vote-pct (if (> total-votes u0) (/ (* branch-votes u100) total-votes) u0))
            )
                (map-set branch-analytics {episode-id: episode-id, branch-id: branch-id}
                    {
                        vote-percentage: vote-pct,
                        unique-voters: u0,
                        largest-single-vote: u0,
                        average-vote-weight: u0
                    })
                (ok true)
            )
            (ok true)
        )
    )
)

(define-private (update-community-metrics (episode-id uint))
    (let (
        (participants (get-episode-participants episode-id))
        (participant-count (len participants))
        (episode-analytics-data (unwrap! (map-get? episode-analytics episode-id) ERR_ANALYTICS_NOT_FOUND))
        (consensus (get consensus-strength episode-analytics-data))
    )
        (map-set community-metrics episode-id
            {
                active-voters-count: participant-count,
                engagement-rate: u100,
                average-consensus: consensus,
                total-activity: (get total-tokens-voted episode-analytics-data)
            })
        (ok true)
    )
)

(define-read-only (get-episode-analytics (episode-id uint))
    (map-get? episode-analytics episode-id)
)

(define-read-only (get-branch-analytics (episode-id uint) (branch-id uint))
    (map-get? branch-analytics {episode-id: episode-id, branch-id: branch-id})
)

(define-read-only (get-user-voting-history (user principal))
    (map-get? user-voting-history user)
)

(define-read-only (get-community-metrics (episode-id uint))
    (map-get? community-metrics episode-id)
)

(define-read-only (get-platform-statistics)
    (ok {
        total-votes-cast: (var-get total-votes-cast),
        total-episodes-completed: (var-get total-episodes-completed),
        total-unique-voters: (var-get total-unique-voters),
        total-reputation-distributed: (var-get total-reputation-distributed),
        reward-pool-balance: (var-get reward-pool-balance)
    })
)

(define-read-only (get-episode-comparison (episode-id-1 uint) (episode-id-2 uint))
    (let (
        (analytics-1 (map-get? episode-analytics episode-id-1))
        (analytics-2 (map-get? episode-analytics episode-id-2))
    )
        (ok {
            episode-1: analytics-1,
            episode-2: analytics-2,
            participation-diff: (if (and (is-some analytics-1) (is-some analytics-2))
                (if (> (get total-participants (unwrap-panic analytics-1)) (get total-participants (unwrap-panic analytics-2)))
                    (- (get total-participants (unwrap-panic analytics-1)) (get total-participants (unwrap-panic analytics-2)))
                    (- (get total-participants (unwrap-panic analytics-2)) (get total-participants (unwrap-panic analytics-1))))
                u0)
        })
    )
)

(define-read-only (get-branch-popularity-ranking (episode-id uint))
    (let (
        (branch-1 (map-get? branch-analytics {episode-id: episode-id, branch-id: u1}))
        (branch-2 (map-get? branch-analytics {episode-id: episode-id, branch-id: u2}))
        (branch-3 (map-get? branch-analytics {episode-id: episode-id, branch-id: u3}))
    )
        (ok {
            branch-1-percentage: (if (is-some branch-1) (get vote-percentage (unwrap-panic branch-1)) u0),
            branch-2-percentage: (if (is-some branch-2) (get vote-percentage (unwrap-panic branch-2)) u0),
            branch-3-percentage: (if (is-some branch-3) (get vote-percentage (unwrap-panic branch-3)) u0)
        })
    )
)

(define-read-only (get-user-engagement-score (user principal))
    (let (
        (history (map-get? user-voting-history user))
        (reputation (get-user-reputation-data user))
    )
        (match history
            user-data
            (ok {
                participation-score: (* (get episodes-participated user-data) u10),
                accuracy-score: (get-voting-accuracy user),
                reputation-score: (get total-reputation reputation),
                total-engagement: (+ (* (get episodes-participated user-data) u10) (get total-reputation reputation))
            })
            (ok {
                participation-score: u0,
                accuracy-score: u0,
                reputation-score: (get total-reputation reputation),
                total-engagement: u0
            })
        )
    )
)

(define-read-only (get-consensus-trend (start-episode uint) (end-episode uint))
    (let (
        (start-analytics (map-get? episode-analytics start-episode))
        (end-analytics (map-get? episode-analytics end-episode))
    )
        (ok {
            start-consensus: (if (is-some start-analytics) (get consensus-strength (unwrap-panic start-analytics)) u0),
            end-consensus: (if (is-some end-analytics) (get consensus-strength (unwrap-panic end-analytics)) u0),
            trend-direction: (if (and (is-some start-analytics) (is-some end-analytics))
                (if (> (get consensus-strength (unwrap-panic end-analytics)) (get consensus-strength (unwrap-panic start-analytics)))
                    "increasing"
                    "decreasing")
                "unknown")
        })
    )
)

(map-set token-balances CONTRACT_OWNER (var-get total-token-supply))
