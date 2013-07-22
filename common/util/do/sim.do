view wave
add wave -r *
set broken 0
onbreak {
    set broken 1
    resume
}
run -all
if { $broken } {
    quit -code 1
} else {
    quit -code 0
}
