source /usr/share/gef/gef.py

# This is GEF's default context layout, but for source-based debugging you of
# course won't need most of this
define gef-default
  gef config context.layout "legend regs stack code args source memory threads trace extra"
end

define gef-minimal
  gef config context.layout "trace extra source"
end

gef-minimal

# Enable gef-extras, tilde expansion doesn't work here
gef config context.libc_args True
gef config context.libc_args_path /home/robbert/.local/share/gef-extras/glibc-function-args
gef config gef.extra_plugins_dir /home/robbert/.local/share/gef-extras/scripts
gef config pcustom.struct_path /home/robbert/.local/share/gef-extras/structs
gef config syscall-args.path /home/robbert/.local/share/gef-extras/syscall-tables

# Steal the prompts from gdb-dashboard, since I like those a lot better
python GEF_PROMPT_ON = "\001\033[1;35m\002>>>\001\033[0m\002 "
python GEF_PROMPT_OFF = "\001\033[1;30m\002>>>\001\033[0m\002 "

# GEF already sets most settings to how I like them
set confirm on
