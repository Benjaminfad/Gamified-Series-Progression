(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_EPISODE (err u101))
(define-constant ERR_VOTING_CLOSED (err u102))
(define-constant ERR_ALREADY_VOTED (err u103))
(define-constant ERR_INSUFFICIENT_TOKENS (err u104))
(define-constant ERR_EPISODE_NOT_FOUND (err u105))
(define-constant ERR_BRANCH_NOT_FOUND (err u106))
(define-constant ERR_VOTING_STILL_ACTIVE (err u107))

(define-data-var next-episode-id uint u1)
(define-data-var total-token-supply uint u1000000)
(define-data-var voting-duration uint u144)

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

(map-set token-balances CONTRACT_OWNER (var-get total-token-supply))
