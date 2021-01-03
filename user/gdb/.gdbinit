# This is for GEF
source /usr/share/gef/gef.py

# This is GEF's default context layout, but for source-based debugging you of
# course won't need most of this
define gef-default
  gef config context.layout "legend regs stack code args source memory threads trace extra"
end

define gef-minimal
  gef config context.layout "args source trace extra"
end

gef-minimal

# Steal the prompts from gdb-dashboard, since I like those a lot better
python GEF_PROMPT_ON = "\001\033[1;35m\002>>>\001\033[0m\002 "
python GEF_PROMPT_OFF = "\001\033[1;30m\002>>>\001\033[0m\002 "

# # This is for gdb-dashboard
# source /usr/share/gdb-dashboard/.gdbinit

# # This shows everything on the dashboard in a logical order it's quite cluttered
# define db-everything
#   dashboard -l registers threads stack assembly memory history expressions breakpoints variables source
# end

# define db-basic
#   dashboard -l history breakpoints expressions variables source
# end

# # Same as the above without showing all locals since formatting that can be
# # quite slow. It's quite useful, but `info locals` and `dashboard expression
# # watch` are almost as convenient and much faster.
# define db-minimal
#   dashboard -l history breakpoints expressions source
# end

# db-minimal

set disassembly-flavor intel
