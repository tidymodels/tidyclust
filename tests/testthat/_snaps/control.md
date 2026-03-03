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

# print.control_cluster() works

    Code
      print(control_cluster())
    Output
      tidyclust control object

---

    Code
      print(control_cluster(verbosity = 2))
    Output
      tidyclust control object
       - verbose level 2 

---

    Code
      print(control_cluster(catch = TRUE))
    Output
      tidyclust control object
       - fit errors will be caught

---

    Code
      print(control_cluster(verbosity = 2, catch = TRUE))
    Output
      tidyclust control object
       - verbose level 2 
       - fit errors will be caught

