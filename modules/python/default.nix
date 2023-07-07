{ pkgs, ... }:

let
  # See below
  qtPluginPathMakeWrapperArgs = with pkgs.qt5; [
    "--set"
    "QT_PLUGIN_PATH"
    "${qtbase}/${qtbase.qtPluginPrefix}"
  ];

  # These Python packages aren't packaged in nixpkgs
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

  pythonEnv = ((pkgs.python310.override {
    packageOverrides = self: super: {
      # Neither Qt nor TK works out of the box. I figured out a workaround to
      # get the Qt backend to work involving the `qt5-run` wrapper defined
      # below, so that's what I'm using for now.
      matploblib = super.matplotlib.override {
        enableQt = true;
        enableTk = false;
      };

      ipython = super.ipython.overridePythonAttrs (old: {
        # HACK: Workaround needed to set the correct Qt plugin path so
        #       matplotlib works in IPython:
        #       https://github.com/NixOS/nixpkgs/issues/80147#issuecomment-784857897
        makeWrapperArgs = (old.makeWrapperArgs or [ ])
          ++ qtPluginPathMakeWrapperArgs;
      });
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
    ]));
in {
  home.packages = [ pythonEnv ];

  # The system installed Google Cloud environment cannot find NumPy by default
  # since it uses its own bundled Python interpreter. As a workaround we can
  # tell it to use our own Python interpreter which does include NumPy. It will
  # be fine!
  home.sessionVariables = {
    CLOUDSDK_PYTHON_SITEPACKAGES = 1;
    CLOUDSDK_PYTHON = "${pythonEnv}/bin/python3";
  };

  xdg.configFile."flake8".source = ./flake8;
  xdg.configFile."pylintrc".source = ./pylintrc;
}
