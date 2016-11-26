{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.services.curlpload;

  curlpload-package = import ./shell.nix {};

  curlpload-config = pkgs.writeText "curlpload.ini" ''
    [Server]
    host=${cfg.server.host}
    ${optionalString (cfg.server.port != null) "port=${toString cfg.server.port}"}
    ${optionalString (cfg.server.lifetime != null) "lifetime=${toString cfg.server.lifetime}"}

    [Database]
    ${optionalString (cfg.database.host != null) "host=${toString cfg.database.host}"}
    ${optionalString (cfg.database.port != null) "port=${toString cfg.database.port}"}
    name=${cfg.database.name}
    user=${cfg.database.user}
    ${optionalString (cfg.database.password != null) "password=${toString cfg.database.password}"}

    [Uploads]
    ${optionalString (cfg.uploads.path != null) "path=${cfg.uploads.path}"}
    ${optionalString (cfg.uploads.keep-names != null)
       "keep_names=${if cfg.uploads.keep-names then "true" else "false"}"}
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
            External address of the service.
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

        lifetime = mkOption {
          type = types.nullOr types.int;
          default = null;
          example = 10;
          description = ''
            After which period of inactivity to shut down the server.
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
            Path to a file with the database user's password.
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
          chmod 755 "${cfg.uploads.path}"
        '' + optionalString (cfg.database.password != null) ''
          chown "${cfg.user}" "${cfg.database.password}"
          chmod 400 "${cfg.database.password}"
        '';

        serviceConfig = {
          User = "${cfg.user}";
          ExecStart = "${curlpload-package}/bin/curlpload -c ${curlpload-config}";
          StandardOutput = "syslog";
          StandardError = "syslog";
          PermissionsStartOnly = true;
          Restart = "no";
        };

        wantedBy = [ "multi-user.target" ];
        requires = ["postgresql.service"];
        after = ["network.target" "postgresql.service"];
    };
  };
}
