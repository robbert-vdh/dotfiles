# This is where the gdb-dashboard package on Arch/Manjaro installs
# gdb-dashboard's .gdbinit file
source /usr/share/gdb-dashboard/.gdbinit

# This shows everything on the dashboard in a logical order it's quite cluttered
define db-everything
  dashboard -l registers threads stack assembly memory history expressions breakpoints variables source
end

define db-basic
  dashboard -l history breakpoints expressions variables source
end

# Same as the above without showing all locals since formatting that can be
# quite slow. It's quite useful, but `info locals` and `dashboard expression
# watch` are almost as convenient and much faster.
define db-minimal
  dashboard -l history breakpoints expressions source
end

set disassembly-flavor intel
db-minimal
