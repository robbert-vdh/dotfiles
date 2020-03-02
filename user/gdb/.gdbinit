# This is where the gdb-dashboard package on Arch/Manjaro installs
# gdb-dashboard's .gdbinit file
source /usr/share/gdb-dashboard/.gdbinit

# This shows everything on the dashboard in a logical order, but doing so is
# quite slow since it has to fetch a lot of information every time you step
define db-everything
  dashboard -l registers threads stack assembly memory history expressions breakpoints variables source
end

# And by default only show the essentials
define db-simple
  dashboard -l history breakpoints expressions variables source
end

set disassembly-flavor intel
db-simple
