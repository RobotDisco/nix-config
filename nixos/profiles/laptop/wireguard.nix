{
  # Since I use wireguard to route all traffic to my home VPN
  # we need to tell my firewall that is ok for the firewall's
  # reverse path filter to ignore wireguard traffic
  networking.firewall = {
    # log packets that are dropped
    logReversePathDrops = true;

    extraCommands = ''
      ip46tables -t mangle -I nixos-fw-rpfilter -p udp -m udp --sport 20990 -j RETURN
      ip46tables -t mangle -I nixos-fw-rpfilter -p udp -m udp --dport 20990 -j RETURN
    '';
    extraStopCommands = ''
      ip46tables -t mangle -D nixos-fw-rpfilter -p udp -m udp --sport 20990 -j RETURN || true
      ip46tables -t mangle -D nixos-fw-rpfilter -p udp -m udp --dport 20990 -j RETURN || true
    '';
  };
}
