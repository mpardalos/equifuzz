set_custom_solve_script "orch_multipliers"
set_user_assumes_lemmas_procedure "miter"

create_design -name spec -top mod1 -lang mx
vcs -sverilog mod1.v
compile_design spec

create_design -name impl -top mod2 -lang mx
vcs -sverilog mod2.v
compile_design impl

proc miter {} {
        map_by_name -inputs -implphase 1 -specphase 1
        map_by_name -outputs -implphase 1 -specphase 1
}

compose
solveNB proof
proofwait
listproof
quit