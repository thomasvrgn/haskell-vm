add:
  ADD
  PUSH 5
  ADD
  STORE f

main:
  LOAD_SECTION add
  LOAD f
  PUSH 5
  PUSH 6
  CALL 2