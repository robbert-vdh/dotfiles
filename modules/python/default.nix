{ pkgs, ... }:

# These Python packages aren't packaged in nixpkgs
let
  mkJupyterlab_code_formatter = ps:
    with ps;
    (buildPythonPackage rec {
      pname = "jupyterlab_code_formatter";
      version = "2.0.0";
      format = "pyproject";

      src = fetchPypi {
        inherit pname version;
        sha256 = "gGWqAPLmes6PqdKLYq1NPow23uL3TlOg0SKkHq0ixew=";
      };

      buildInputs =
        [ hatchling hatch-jupyter-builder hatch-nodejs-version jupyterlab ];
    });
in {
  home.packages = [
    ((pkgs.python310.override {
      packageOverrides = self: super: {
        # Neither Qt nor TK works out of the box. I figured out a workaround to
        # get the Qt backend to work involving the `qt5-run` wrapper defined
        # below, so that's what I'm using for now.
        matploblib = super.matplotlib.override {
          enableQt = true;
          enableTk = false;
        };
      };
    }).withPackages (ps:
      with ps; [
        # Binaries
        pgcli
        ipython
        jupyterlab

        # Formatters and linters
        black
        (mkJupyterlab_code_formatter ps)
        isort

        # Libs
        librosa
        matplotlib
        numpy
        pandas
        pyperclip
        pyqt5 # Needed for matplotlib
        scipy
        seaborn
        statsmodels
      ]))

    # HACK: Workaround needed to set the correct Qt plugin path so matplotlib
    #       works in IPython:
    #       https://github.com/NixOS/nixpkgs/issues/80147#issuecomment-784857897
    # TODO: Modify the IPython package to do this in the wrapper
    (pkgs.writeShellScriptBin "qt5-run" (with pkgs.qt5; ''
      export QT_PLUGIN_PATH=${qtbase}/${qtbase.qtPluginPrefix}
      if [[ $# -eq 0 ]]; then
        echo >&2 "Usage: qt5-run <command...>"
        exit 1
      fi

      exec "$@"
    ''))
  ];

  xdg.configFile."flake8".source = ./flake8;
  xdg.configFile."pylintrc".source = ./pylintrc;
}
