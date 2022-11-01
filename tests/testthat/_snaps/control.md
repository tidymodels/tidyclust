# control_cluster() error with wrong input

    Code
      control_cluster(verbosity = 5.5)
    Condition
      Error in `control_cluster()`:
      ! verbosity should be an integer.

---

    Code
      control_cluster(verbosity = "3")
    Condition
      Error in `control_cluster()`:
      ! verbosity should be an integer.

---

    Code
      control_cluster(catch = "yes")
    Condition
      Error in `control_cluster()`:
      ! catch should be a logical.

