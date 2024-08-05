{ robotdisco-secrets, ... }:
let
  s = robotdisco-secrets;
in
{
  age.secrets = {
    upsmon-primary.file = "${s}/ups-user-primary.age";
    upsmon-secondary.file = "${s}/ups-user-secondary.age";
  };
}
