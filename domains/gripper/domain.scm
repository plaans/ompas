(begin
  (define ompas_path (get-env-var "OMPAS_PATH"))
  (define gripper_path (concatenate ompas_path "/domains/gripper/"))
  (load (concatenate gripper_path "base.scm"))
  (load (concatenate gripper_path "om.scm"))
)