(begin
    (exec-command 'move (read-state 'at-robby) (read-state 'at ?ball))
    (exec-command 'pick (read-state 'at ?ball) ?gripper)))