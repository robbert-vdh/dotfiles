{ pkgs, ... }:

let
  # These Python packages aren't packaged in nixpkgs
  mkJupyterlab_code_formatter = ps:
    with ps;
    (buildPythonPackage rec {
      pname = "jupyterlab_code_formatter";
      version = "2.2.1";
      format = "pyproject";

      src = fetchPypi {
        inherit pname version;
        sha256 = "hTIoGdph8CXrx8SoQpeZlPqBKtRMM0/bbrueutWNrgg=";
      };

      buildInputs =
        [ hatchling hatch-jupyter-builder hatch-nodejs-version jupyterlab ];
    });

  # git-blame this for a wrapper that fixes matplotlib+Qt
  # NOTE: This implicit dependency hangs on the check phase
  #       (https://github.com/NixOS/nixpkgs/issues/262000) without any overrides
  pythonOverlay = self: super: {
    debugpy =
      super.debugpy.overrideAttrs (self: super: { pytestCheckPhase = "true"; });
  };

  pythonEnv =
    (pkgs.python310.override { packageOverrides = pythonOverlay; }).withPackages
    (ps:
      with ps; [
        # Binaries
        ipython
        jupyterlab
        pgcli

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
        scipy
        seaborn
        statsmodels
      ]);
in {
  home.packages = [ pythonEnv pkgs.nbstripout ];

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
