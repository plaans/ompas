(begin
  (define ompas_path (get-env-var "OMPAS_PATH"))
  (define gripper_path (concatenate ompas_path "/domains/gripper/"))
  (read (concatenate gripper_path "base.lisp"))
  (read (concatenate gripper_path "om.lisp"))
)