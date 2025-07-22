;; Cross-Game Asset Protocol
;; A protocol for managing and transferring game assets across multiple games
;; Version: 1.0.0
;; Author: Ajiboh3
;; This contract enables seamless asset interoperability between games
;; Built on Stacks blockchain for maximum security and decentralization
;; Error codes
(define-constant ERR-NOT-AUTHORIZED (err u1001))
(define-constant ERR-ASSET-NOT-FOUND (err u1002))
(define-constant ERR-INSUFFICIENT-BALANCE (err u1003))
(define-constant ERR-INVALID-RECIPIENT (err u1004))
(define-constant ERR-GAME-NOT-REGISTERED (err u1005))
(define-constant ERR-ASSET-ALREADY-EXISTS (err u1006))
(define-constant ERR-INVALID-AMOUNT (err u1007))
(define-constant ERR-TRANSFER-FAILED (err u1008))
(define-constant ERR-ACHIEVEMENT-NOT-FOUND (err u1009))
(define-constant ERR-ACHIEVEMENT-ALREADY-UNLOCKED (err u1010))

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Data structures
(define-map games 
    { game-id: uint } 
    { 
        name: (string-ascii 50),
        developer: principal,
        is-active: bool,
        asset-count: uint,
        registration-block: uint
    }
)

(define-map assets 
    { asset-id: uint } 
    { 
        name: (string-ascii 100),
        description: (string-ascii 500),
        asset-type: (string-ascii 20),
        rarity: (string-ascii 20),
        origin-game: uint,
        creator: principal,
        total-supply: uint,
        is-transferable: bool,
        metadata-uri: (optional (string-ascii 200))
    }
)

(define-map user-assets 
    { user: principal, asset-id: uint } 
    { balance: uint }
)

(define-map achievements
    { achievement-id: uint }
    {
        name: (string-ascii 100),
        description: (string-ascii 500),
        game-id: uint,
        points: uint,
        rarity: (string-ascii 20),
        requirements: (string-ascii 300)
    }
)

(define-map user-achievements
    { user: principal, achievement-id: uint }
    {
        unlocked-at: uint,
        verification-data: (string-ascii 200)
    }
)

(define-map cross-game-compatibility
    { from-game: uint, to-game: uint, asset-id: uint }
    { is-compatible: bool, conversion-rate: uint }
)

;; Data variables
(define-data-var next-game-id uint u1)
(define-data-var next-asset-id uint u1)
(define-data-var next-achievement-id uint u1)
(define-data-var protocol-fee uint u100) ;; Fee in micro-STX
(define-data-var total-games uint u0)
(define-data-var total-assets uint u0)

;; Game registration functions
(define-public (register-game (name (string-ascii 50)) (developer principal))
    (let 
        (
            (game-id (var-get next-game-id))
        )
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (map-set games 
            { game-id: game-id }
            {
                name: name,
                developer: developer,
                is-active: true,
                asset-count: u0,
                registration-block: block-height
            }
        )
        (var-set next-game-id (+ game-id u1))
        (var-set total-games (+ (var-get total-games) u1))
        (ok game-id)
    )
)

(define-public (deactivate-game (game-id uint))
    (let 
        (
            (game-data (unwrap! (map-get? games { game-id: game-id }) ERR-GAME-NOT-REGISTERED))
        )
        (asserts! (or (is-eq tx-sender CONTRACT-OWNER) (is-eq tx-sender (get developer game-data))) ERR-NOT-AUTHORIZED)
        (map-set games 
            { game-id: game-id }
            (merge game-data { is-active: false })
        )
        (ok true)
    )
)

;; Asset creation and management
(define-public (create-asset 
    (name (string-ascii 100))
    (description (string-ascii 500))
    (asset-type (string-ascii 20))
    (rarity (string-ascii 20))
    (origin-game uint)
    (total-supply uint)
    (is-transferable bool)
    (metadata-uri (optional (string-ascii 200)))
)
    (let 
        (
            (asset-id (var-get next-asset-id))
            (game-data (unwrap! (map-get? games { game-id: origin-game }) ERR-GAME-NOT-REGISTERED))
        )
        (asserts! (> total-supply u0) ERR-INVALID-AMOUNT)
        (asserts! (get is-active game-data) ERR-GAME-NOT-REGISTERED)
        (asserts! (or (is-eq tx-sender CONTRACT-OWNER) (is-eq tx-sender (get developer game-data))) ERR-NOT-AUTHORIZED)
        
        (map-set assets 
            { asset-id: asset-id }
            {
                name: name,
                description: description,
                asset-type: asset-type,
                rarity: rarity,
                origin-game: origin-game,
                creator: tx-sender,
                total-supply: total-supply,
                is-transferable: is-transferable,
                metadata-uri: metadata-uri
            }
        )
        
        ;; Mint initial supply to creator
        (map-set user-assets 
            { user: tx-sender, asset-id: asset-id }
            { balance: total-supply }
        )
        
        ;; Update game asset count
        (map-set games 
            { game-id: origin-game }
            (merge game-data { asset-count: (+ (get asset-count game-data) u1) })
        )
        
        (var-set next-asset-id (+ asset-id u1))
        (var-set total-assets (+ (var-get total-assets) u1))
        (ok asset-id)
    )
)

;; Asset transfer functions
(define-public (transfer-asset (asset-id uint) (amount uint) (recipient principal))
    (let 
        (
            (asset-data (unwrap! (map-get? assets { asset-id: asset-id }) ERR-ASSET-NOT-FOUND))
            (sender-balance (default-to u0 (get balance (map-get? user-assets { user: tx-sender, asset-id: asset-id }))))
            (recipient-balance (default-to u0 (get balance (map-get? user-assets { user: recipient, asset-id: asset-id }))))
        )
        (asserts! (> amount u0) ERR-INVALID-AMOUNT)
        (asserts! (get is-transferable asset-data) ERR-TRANSFER-FAILED)
        (asserts! (>= sender-balance amount) ERR-INSUFFICIENT-BALANCE)
        (asserts! (not (is-eq tx-sender recipient)) ERR-INVALID-RECIPIENT)
        
        ;; Update sender balance
        (if (is-eq sender-balance amount)
            (map-delete user-assets { user: tx-sender, asset-id: asset-id })
            (map-set user-assets 
                { user: tx-sender, asset-id: asset-id }
                { balance: (- sender-balance amount) }
            )
        )
        
        ;; Update recipient balance
        (map-set user-assets 
            { user: recipient, asset-id: asset-id }
            { balance: (+ recipient-balance amount) }
        )
        
        (ok true)
    )
)

;; Cross-game compatibility functions
(define-public (set-cross-game-compatibility 
    (from-game uint) 
    (to-game uint) 
    (asset-id uint) 
    (is-compatible bool) 
    (conversion-rate uint)
)
    (let 
        (
            (from-game-data (unwrap! (map-get? games { game-id: from-game }) ERR-GAME-NOT-REGISTERED))
            (to-game-data (unwrap! (map-get? games { game-id: to-game }) ERR-GAME-NOT-REGISTERED))
            (asset-data (unwrap! (map-get? assets { asset-id: asset-id }) ERR-ASSET-NOT-FOUND))
        )
        (asserts! (or (is-eq tx-sender CONTRACT-OWNER) 
                     (is-eq tx-sender (get developer from-game-data))
                     (is-eq tx-sender (get developer to-game-data))) ERR-NOT-AUTHORIZED)
        
        (map-set cross-game-compatibility
            { from-game: from-game, to-game: to-game, asset-id: asset-id }
            { is-compatible: is-compatible, conversion-rate: conversion-rate }
        )
        (ok true)
    )
)

;; Achievement system
(define-public (create-achievement
    (name (string-ascii 100))
    (description (string-ascii 500))
    (game-id uint)
    (points uint)
    (rarity (string-ascii 20))
    (requirements (string-ascii 300))
)
    (let 
        (
            (achievement-id (var-get next-achievement-id))
            (game-data (unwrap! (map-get? games { game-id: game-id }) ERR-GAME-NOT-REGISTERED))
        )
        (asserts! (or (is-eq tx-sender CONTRACT-OWNER) (is-eq tx-sender (get developer game-data))) ERR-NOT-AUTHORIZED)
        (asserts! (get is-active game-data) ERR-GAME-NOT-REGISTERED)
        
        (map-set achievements
            { achievement-id: achievement-id }
            {
                name: name,
                description: description,
                game-id: game-id,
                points: points,
                rarity: rarity,
                requirements: requirements
            }
        )
        
        (var-set next-achievement-id (+ achievement-id u1))
        (ok achievement-id)
    )
)

(define-public (unlock-achievement (achievement-id uint) (user principal) (verification-data (string-ascii 200)))
    (let 
        (
            (achievement-data (unwrap! (map-get? achievements { achievement-id: achievement-id }) ERR-ACHIEVEMENT-NOT-FOUND))
            (game-data (unwrap! (map-get? games { game-id: (get game-id achievement-data) }) ERR-GAME-NOT-REGISTERED))
        )
        (asserts! (or (is-eq tx-sender CONTRACT-OWNER) (is-eq tx-sender (get developer game-data))) ERR-NOT-AUTHORIZED)
        (asserts! (is-none (map-get? user-achievements { user: user, achievement-id: achievement-id })) ERR-ACHIEVEMENT-ALREADY-UNLOCKED)
        
        (map-set user-achievements
            { user: user, achievement-id: achievement-id }
            {
                unlocked-at: block-height,
                verification-data: verification-data
            }
        )
        (ok true)
    )
)

;; Read-only functions
(define-read-only (get-game (game-id uint))
    (map-get? games { game-id: game-id })
)

(define-read-only (get-asset (asset-id uint))
    (map-get? assets { asset-id: asset-id })
)

(define-read-only (get-user-asset-balance (user principal) (asset-id uint))
    (default-to u0 (get balance (map-get? user-assets { user: user, asset-id: asset-id })))
)

(define-read-only (get-achievement (achievement-id uint))
    (map-get? achievements { achievement-id: achievement-id })
)

(define-read-only (has-user-achievement (user principal) (achievement-id uint))
    (is-some (map-get? user-achievements { user: user, achievement-id: achievement-id }))
)

(define-read-only (get-cross-game-compatibility (from-game uint) (to-game uint) (asset-id uint))
    (map-get? cross-game-compatibility { from-game: from-game, to-game: to-game, asset-id: asset-id })
)

(define-read-only (get-protocol-stats)
    {
        total-games: (var-get total-games),
        total-assets: (var-get total-assets),
        next-game-id: (var-get next-game-id),
        next-asset-id: (var-get next-asset-id),
        next-achievement-id: (var-get next-achievement-id),
        protocol-fee: (var-get protocol-fee)
    }
)

;; Administrative functions
(define-public (set-protocol-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set protocol-fee new-fee)
        (ok true)
    )
)

(define-public (emergency-pause-game (game-id uint))
    (let 
        (
            (game-data (unwrap! (map-get? games { game-id: game-id }) ERR-GAME-NOT-REGISTERED))
        )
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (map-set games 
            { game-id: game-id }
            (merge game-data { is-active: false })
        )
        (ok true)
    )
)