{ inputs, pkgs, ... }:

{
  home.packages = [ pkgs.gdb ];

  xdg.configFile."gdb/gdbinit".text = ''
    # gdb-dashboard is included through Nix
    source ${inputs.gdb-dashboard}/.gdbinit

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

    define db-enable
      dashboard -enabled on
    end

    define db-disable
      dashboard -enabled off
    end

    set disassembly-flavor intel
    set debuginfod enabled on

    # Start with gdb-dashboard disabled because its clearing behavior makes it very
    # easy to throw away useful information. Still no way to disable that.
    db-minimal
    db-disable
  '';
}
