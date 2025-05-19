{
  services.v2ray = {
    enable = true;
    config = {
      inbounds = [
        {
          port = 1080;
          protocol = "socks";
        }
        {
          port = 8123;
          protocol = "http";
        }
      ];
      outbounds = [
        {
          protocol = "vmess";
          settings = {
            vnext = [
              {
                address = "ss";
                port = 55663;
                users = [
                  {
                    id = "2055644b-4af9-4eb2-9eca-7ec023e30ead";
                    alterId = 0;
                  }
                ];
              }
            ];
          };
        }
      ];
    };
  };

  networking = {
    proxy = {
      default = "http://127.0.0.1:8123";
      noProxy = "127.0.0.1,localhost,internal.domain,.cn,.aliyun.com";
    };
    extraHosts = ''
      216.128.176.101 ss
    '';
  };
}
