{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.services.curlpload;

  curlpload-package = import ./shell.nix {};

  curlpload-config = pkgs.writeText "curlpload.ini" ''
    [Server]
    host=${cfg.server.host}
    ${optionalString (cfg.server.port != null) "port=${toString cfg.server.port}"}

    [Database]
    ${optionalString (cfg.database.host != null) "host=${toString cfg.database.host}"}
    name=${cfg.database.name}
    user=${cfg.database.user}
    ${optionalString (cfg.database.port != null) "password=${toString cfg.database.port}"}

    [Uploads]
    ${optionalString (cfg.uploads.path != null) "path=${cfg.uploads.path}"}
    ${optionalString (cfg.uploads.keep-names != null) "keep_names=${toString cfg.uploads.keep-names}"}
  '';
 in {

  ###### interface

  options = {

    services.curlpload = {

      enable = mkEnableOption "curlpload";

      user = mkOption {
        type = types.str;
        default = "nginx";
        example = "nginx";
        description = ''
          User account under which the applicaton runs.
        '';
      };

      server = {
        host = mkOption {
          type = types.str;
          default = "localhost";
          description = ''
            TODO
          '';
        };

        port = mkOption {
         type = types.nullOr types.int;
         default = null;
         example = 8080;
         description = ''
           Port of the application.
         '';
        };
      };

      database = {
        host = mkOption {
          type = types.nullOr types.str;
          default = null;
          description = ''
            Host of the database. Leave null to use Unix domain socket.
          '';
        };

        port = mkOption {
          type = types.nullOr types.int;
          default = null;
          description = ''
            Port of the database. Leave null to use default one.
          '';
        };

        name = mkOption {
          type = types.str;
          default = "curlpload";
          description = ''
            Name of the existing database.
          '';
        };

        user = mkOption {
          type = types.str;
          default = "curlpload";
          description = ''
            The database user. The user must exist and has access to
            the specified database.
          '';
        };

        password = mkOption {
          type = types.nullOr types.str;
          default = null;
          description = ''
            The database user's password.
          '';
        };
      };

      uploads = {
        path = mkOption {
          type = types.nullOr types.str;
          default = null;
          description = ''
            Where to store uploaded files. When is null, temporary directory will be created.
          '';
        };

        keep-names = mkOption {
          type = types.bool;
          default = false;
          description = ''
            Whether to keep original files names.
          '';
        };
      };
    };
  };


  ###### implementation

  config = mkIf cfg.enable {

    systemd.services.curlpload = {

        description = "Curlpload service";

        preStart = optionalString (cfg.uploads.path != null) ''
          mkdir -p "${cfg.uploads.path}"
          chown -R "${cfg.user}" "${cfg.uploads.path}"
          chmod -R 644 "${cfg.uploads.path}"
        '';

        serviceConfig = {
          User = "${cfg.user}";
          ExecStart = "${curlpload-package}/bin/curlpload -c ${curlpload-config}";
          StandardOutput = "syslog";
          StandardError = "syslog";
          PermissionsStartOnly = true;
        };

        wantedBy = [ "multi-user.target" ];
        requires = ["postgresql.service"];
        after = ["network.target" "postgresql.service"];
    };
  };
}