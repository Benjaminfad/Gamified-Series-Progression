# 🎬 Gamified Series Progression

A blockchain-based interactive storytelling platform where token holders vote on TV show plot branches, determining the script direction in real-time! 🚀

## 📖 Overview

This Clarity smart contract enables a revolutionary entertainment experience where viewers become active participants in shaping their favorite shows. Token holders can vote on different plot branches for each episode, and the winning option determines how the story unfolds.

## ✨ Key Features

- 🗳️ **Token-based Voting System**: Use tokens to vote on plot directions
- 📺 **Episode Management**: Create and manage TV show episodes
- 🌳 **Plot Branches**: Multiple story paths for each episode
- 🏆 **Democratic Outcomes**: Highest voted branch wins and shapes the narrative
- 👥 **Community Participation**: Track all participants in each episode's voting
- ⏰ **Time-limited Voting**: Episodes have specific voting windows

## 🔧 Core Functions

### 📊 Token Management
- `transfer-tokens`: Transfer tokens between users
- `distribute-tokens`: Owner distributes tokens to multiple recipients
- `get-token-balance`: Check user's token balance

### 🎭 Episode Management
- `create-episode`: Create new episode with title and description
- `add-plot-branch`: Add plot options for episodes
- `finalize-episode`: Close voting and determine winner
- `get-episode-info`: Get episode details

### 🗳️ Voting System
- `vote-on-branch`: Vote on a plot branch using tokens
- `is-voting-active`: Check if voting is still open
- `get-user-vote`: See user's vote for an episode

### 📈 Analytics
- `get-episode-participants`: List all voters for an episode
- `get-branch-info`: Get vote counts for plot branches
- `get-current-episode-id`: Get the latest episode ID

## 🚀 Usage Guide

### 1. Initial Setup (Owner Only)
```clarity
;; Distribute tokens to viewers
(distribute-tokens 
    (list 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM 'ST1SJ3DTE5DN7X54YDH5D64R3BCB6A2AG2ZQ8YPD5)
    (list u100 u150)
)

;; Create a new episode
(create-episode "The Mystery Begins" "Our heroes discover a strange artifact")
```

### 2. Add Plot Branches (Owner Only)
```clarity
;; Add different story paths
(add-plot-branch u1 u1 "Investigate Alone" "The protagonist explores the mystery solo")
(add-plot-branch u1 u2 "Team Up" "Form an alliance with other characters")
(add-plot-branch u1 u3 "Seek Expert Help" "Consult a specialist about the artifact")
```

### 3. Community Voting
```clarity
;; Vote on your preferred plot direction
(vote-on-branch u1 u2 u50)  ;; Vote 50 tokens for "Team Up" option
```

### 4. Episode Finalization (Owner Only)
```clarity
;; Close voting and determine the winning branch
(finalize-episode u1)
```

## 🏗️ Contract Architecture

The contract uses several key data structures:

- **Episodes**: Store episode metadata, voting periods, and results
- **Episode Branches**: Different plot options with vote counts
- **User Votes**: Track individual voting choices and token amounts
- **Token Balances**: Manage voting power distribution
- **Participants**: Record community engagement

## ⚙️ Configuration

- **Voting Duration**: Default 144 blocks (~24 hours)
- **Token Supply**: 1,000,000 tokens initially
- **Max Participants**: 100 per episode
- **Max Recipients**: 10 per token distribution

## 🔒 Security Features

- Owner-only functions for episode management
- One vote per user per episode
- Sufficient token balance validation
- Time-based voting restrictions
- Proper error handling with descriptive codes

## 📝 Error Codes

- `u100`: Unauthorized access
- `u101`: Invalid episode
- `u102`: Voting period closed
- `u103`: User already voted
- `u104`: Insufficient tokens
- `u105`: Episode not found
- `u106`: Plot branch not found
- `u107`: Voting still active

## 🎯 Example Workflow

1. 🎬 **Show Creator** deploys contract and distributes tokens to fans
2. 📺 **New Episode** is created with multiple plot branches
3. 🗳️ **Community Votes** using their tokens on preferred storylines
4. ⏰ **Voting Closes** after the specified time period
5. 🏆 **Winning Branch** is determined and becomes canon
6. 📖 **Story Continues** based on community decision

## 🌟 Future Enhancements

- Multi-season support
- Character development voting
- Reward mechanisms for active voters
- Integration with streaming platforms
- NFT rewards for participants

---

*Built with ❤️ on the Stacks blockchain using Clarity smart contracts*
