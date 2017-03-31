-module(utils).

-export([format_timestamp/1, format_timestamp/2, format_mac_addr/1, format_ethernet_proto_type/1, format_ip_proto_type/1, format_ip_addr/1]).

format_timestamp(S, MS) ->
    TS = {S div 1000000, S rem 1000000, MS},
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_universal_time(TS),
    Mstr = element(Month, {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"}),
    io_lib:format("~2w ~s ~4w ~2w:~2..0w:~2..0w.~6..0w", [Day, Mstr, Year, Hour, Minute, Second, MS]).

format_timestamp(S) ->
    format_timestamp(S, 0).

format_mac_addr(<<A:8/unsigned-little-integer, B:8/unsigned-little-integer, C:8/unsigned-little-integer, D:8/unsigned-little-integer, E:8/unsigned-little-integer, F:8/unsigned-little-integer>>) ->
    io_lib:format("~2.16.0B:~2.16.0B:~2.16.0B:~2.16.0B:~2.16.0B:~2.16.0B", [A, B, C, D, E, F]).

format_ethernet_proto_type(Type) ->
    case Type of
        16#0800 -> "Internet Protocol version 4 (IPv4)";
        16#0806 -> "Address Resolution Protocol (ARP)";
        16#0842 -> "Wake-on-LAN[4]";
        16#22F3 -> "IETF TRILL Protocol";
        16#6003 -> "DECnet Phase IV";
        16#8035 -> "Reverse Address Resolution Protocol";
        16#809B -> "AppleTalk (Ethertalk)";
        16#80F3 -> "AppleTalk Address Resolution Protocol (AARP)";
        16#8100 -> "VLAN-tagged frame (IEEE 802.1Q) and Shortest Path Bridging IEEE 802.1aq[5]";
        16#8137 -> "IPX";
        16#8204 -> "QNX Qnet";
        16#86DD -> "Internet Protocol Version 6 (IPv6)";
        16#8808 -> "Ethernet flow control";
        16#8819 -> "CobraNet";
        16#8847 -> "MPLS unicast";
        16#8848 -> "MPLS multicast";
        16#8863 -> "PPPoE Discovery Stage";
        16#8864 -> "PPPoE Session Stage";
        16#887B -> "HomePlug 1.0 MME";
        16#888E -> "EAP over LAN (IEEE 802.1X)";
        16#8892 -> "PROFINET Protocol";
        16#889A -> "HyperSCSI (SCSI over Ethernet)";
        16#88A2 -> "ATA over Ethernet";
        16#88A4 -> "EtherCAT Protocol";
        16#88A8 -> "Provider Bridging (IEEE 802.1ad) & Shortest Path Bridging IEEE 802.1aq[5]";
        16#88AB -> "Ethernet Powerlink[citation needed]";
        16#88B8 -> "GOOSE (Generic Object Oriented Substation event)";
        16#88B9 -> "GSE (Generic Substation Events) Management Services";
        16#88BA -> "SV (Sampled Value Transmission)";
        16#88CC -> "Link Layer Discovery Protocol (LLDP)";
        16#88CD -> "SERCOS III";
        16#88E1 -> "HomePlug AV MME[citation needed]";
        16#88E3 -> "Media Redundancy Protocol (IEC62439-2)";
        16#88E5 -> "MAC security (IEEE 802.1AE)";
        16#88E7 -> "Provider Backbone Bridges (PBB) (IEEE 802.1ah)";
        16#88F7 -> "Precision Time Protocol (PTP) over Ethernet (IEEE 1588)";
        16#88FB -> "Parallel Redundancy Protocol (PRP)";
        16#8902 -> "IEEE 802.1ag Connectivity Fault Management (CFM) Protocol / ITU-T Recommendation Y.1731 (OAM)";
        16#8906 -> "Fibre Channel over Ethernet (FCoE)";
        16#8914 -> "FCoE Initialization Protocol";
        16#8915 -> "RDMA over Converged Ethernet (RoCE)";
        16#891D -> "TTEthernet Protocol Control Frame (TTE)";
        16#892F -> "High-availability Seamless Redundancy (HSR)";
        16#9000 -> "Ethernet Configuration Testing Protocol[6]";
        16#9100 -> "VLAN-tagged (IEEE 802.1Q) frame with double tagging"
    end.

format_ip_proto_type(Type) ->
    case Type of
        0 -> "Reserved [JBP]";
        1 -> "ICMP [53,JBP]";
        3 -> "Gateway-to-Gateway [48,49,VMS]";
        4 -> "CMCC Gateway Monitoring Message [18,19,DFP]";
        5 -> "ST [20,JWF]";
        6 -> "TCP [34,JBP]";
        7 -> "UCL [PK]";
        9 -> "Secure [VGC]";
        10 -> "BBN RCC Monitoring [VMS]";
        11 -> "NVP [12,DC]";
        12 -> "PUP [4,EAT3]";
        13 -> "Pluribus [RDB2]";
        14 -> "Telenet [RDB2]";
        15 -> "XNET [25,JFH2]";
        16 -> "Chaos [MOON]";
        17 -> "User Datagram [42,JBP]";
        18 -> "Multiplexing [13,JBP]";
        19 -> "DCN [DLM1]";
        20 -> "TAC Monitoring [55,RH6]";
        63 -> "any local network [JBP]";
        64 -> "SATNET and Backroom EXPAK [DM11]";
        65 -> "MIT Subnet Support [NC3]";
        69 -> "SATNET Monitoring [DM11]";
        71 -> "Internet Packet Core Utility [DM11]";
        76 -> "Backroom SATNET Monitoring [DM11]";
        78 -> "WIDEBAND Monitoring [DM11]";
        79 -> "WIDEBAND EXPAK [DM11]";
        255 -> "Reserved [JBP]";
        _ -> "Unassigned [JBP]"
    end.

format_ip_addr(<<A:8, B:8, C:8, D:8>>) ->
    io_lib:format("~p.~p.~p.~p", [A, B, C, D]).
