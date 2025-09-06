;; Municipal Utility Billing Smart Contract
;; A comprehensive system for managing water, sewer, and electric utility billing

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u1))
(define-constant ERR_NOT_FOUND (err u2))
(define-constant ERR_INVALID_AMOUNT (err u3))
(define-constant ERR_INSUFFICIENT_PAYMENT (err u4))
(define-constant ERR_ALREADY_PAID (err u5))

;; Service Types
(define-constant SERVICE_WATER u1)
(define-constant SERVICE_SEWER u2)
(define-constant SERVICE_ELECTRIC u3)

;; Data structures
(define-map customer-accounts
  { customer-id: principal }
  {
    name: (string-ascii 50),
    address: (string-ascii 100),
    active: bool
  }
)

(define-map service-meters
  { customer-id: principal, service-type: uint }
  {
    meter-id: (string-ascii 20),
    current-reading: uint,
    last-reading: uint,
    last-read-date: uint
  }
)

(define-map billing-records
  { customer-id: principal, bill-id: uint }
  {
    service-type: uint,
    consumption: uint,
    rate-per-unit: uint,
    total-amount: uint,
    due-date: uint,
    paid: bool,
    payment-date: (optional uint)
  }
)

(define-map service-rates
  { service-type: uint }
  { rate-per-unit: uint }
)

;; Variables
(define-data-var bill-id-nonce uint u0)

;; Private functions
(define-private (is-owner)
  (is-eq tx-sender CONTRACT_OWNER)
)

(define-private (calculate-bill-amount (consumption uint) (rate uint))
  (* consumption rate)
)

(define-private (get-next-bill-id)
  (begin
    (var-set bill-id-nonce (+ (var-get bill-id-nonce) u1))
    (var-get bill-id-nonce)
  )
)

;; Public functions

;; Admin Functions
(define-public (register-customer (customer-id principal) (name (string-ascii 50)) (address (string-ascii 100)))
  (begin
    (asserts! (is-owner) ERR_UNAUTHORIZED)
    (ok (map-set customer-accounts
      { customer-id: customer-id }
      {
        name: name,
        address: address,
        active: true
      }
    ))
  )
)

(define-public (install-meter (customer-id principal) (service-type uint) (meter-id (string-ascii 20)))
  (begin
    (asserts! (is-owner) ERR_UNAUTHORIZED)
    (asserts! (is-some (map-get? customer-accounts { customer-id: customer-id })) ERR_NOT_FOUND)
    (ok (map-set service-meters
      { customer-id: customer-id, service-type: service-type }
      {
        meter-id: meter-id,
        current-reading: u0,
        last-reading: u0,
        last-read-date: u0
      }
    ))
  )
)

(define-public (set-service-rate (service-type uint) (rate uint))
  (begin
    (asserts! (is-owner) ERR_UNAUTHORIZED)
    (asserts! (> rate u0) ERR_INVALID_AMOUNT)
    (ok (map-set service-rates { service-type: service-type } { rate-per-unit: rate }))
  )
)

;; Meter Reading Functions
(define-public (record-meter-reading (customer-id principal) (service-type uint) (new-reading uint) (read-date uint))
  (begin
    (asserts! (is-owner) ERR_UNAUTHORIZED)
    (let ((meter-data (unwrap! (map-get? service-meters { customer-id: customer-id, service-type: service-type }) ERR_NOT_FOUND)))
      (asserts! (>= new-reading (get current-reading meter-data)) ERR_INVALID_AMOUNT)
      (ok (map-set service-meters
        { customer-id: customer-id, service-type: service-type }
        {
          meter-id: (get meter-id meter-data),
          current-reading: new-reading,
          last-reading: (get current-reading meter-data),
          last-read-date: read-date
        }
      ))
    )
  )
)

;; Billing Functions
(define-public (generate-bill (customer-id principal) (service-type uint) (due-date uint))
  (begin
    (asserts! (is-owner) ERR_UNAUTHORIZED)
    (let (
      (meter-data (unwrap! (map-get? service-meters { customer-id: customer-id, service-type: service-type }) ERR_NOT_FOUND))
      (rate-data (unwrap! (map-get? service-rates { service-type: service-type }) ERR_NOT_FOUND))
      (consumption (- (get current-reading meter-data) (get last-reading meter-data)))
      (bill-id (get-next-bill-id))
      (total-amount (calculate-bill-amount consumption (get rate-per-unit rate-data)))
    )
      (ok (map-set billing-records
        { customer-id: customer-id, bill-id: bill-id }
        {
          service-type: service-type,
          consumption: consumption,
          rate-per-unit: (get rate-per-unit rate-data),
          total-amount: total-amount,
          due-date: due-date,
          paid: false,
          payment-date: none
        }
      ))
    )
  )
)

;; Payment Functions
(define-public (pay-bill (customer-id principal) (bill-id uint) (payment-amount uint))
  (begin
    (let ((bill-data (unwrap! (map-get? billing-records { customer-id: customer-id, bill-id: bill-id }) ERR_NOT_FOUND)))
      (asserts! (not (get paid bill-data)) ERR_ALREADY_PAID)
      (asserts! (>= payment-amount (get total-amount bill-data)) ERR_INSUFFICIENT_PAYMENT)
      (ok (map-set billing-records
        { customer-id: customer-id, bill-id: bill-id }
        (merge bill-data {
          paid: true,
          payment-date: (some stacks-block-height)
        })
      ))
    )
  )
)

;; Query Functions
(define-read-only (get-customer-account (customer-id principal))
  (map-get? customer-accounts { customer-id: customer-id })
)

(define-read-only (get-meter-info (customer-id principal) (service-type uint))
  (map-get? service-meters { customer-id: customer-id, service-type: service-type })
)

(define-read-only (get-bill-info (customer-id principal) (bill-id uint))
  (map-get? billing-records { customer-id: customer-id, bill-id: bill-id })
)

(define-read-only (get-service-rate (service-type uint))
  (map-get? service-rates { service-type: service-type })
)

(define-read-only (get-consumption (customer-id principal) (service-type uint))
  (match (map-get? service-meters { customer-id: customer-id, service-type: service-type })
    meter-data (some (- (get current-reading meter-data) (get last-reading meter-data)))
    none
  )
)

(define-read-only (is-bill-paid (customer-id principal) (bill-id uint))
  (match (map-get? billing-records { customer-id: customer-id, bill-id: bill-id })
    bill-data (some (get paid bill-data))
    none
  )
)
