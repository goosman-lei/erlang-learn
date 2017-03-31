-module(pcap).

-export([unpack/1]).

-define(DWORD, 32/unsigned-little-integer).
-define(LONG,  32/unsigned-little-integer).
-define(WORD,  16/unsigned-little-integer).
-define(BYTE,  8/unsigned-little-integer).

unpack(Content) ->
	code:add_patha("./lib"),
    [io:format("~3.16B", [X]) || X <- binary_to_list(Content, 1, 8)], io:format("~n"),

    % unpack for pcap file header(http://www.manpagez.com/man/5/pcap-savefile/)
    io:format("=============================== PCAP PER-FILE HEADER ===============================~n"),
    <<
        16#A1B2C3D4:?DWORD,
        PCAP_FILE_MajorVersion:?WORD,
        PCAP_FILE_MinorVersion:?WORD,
        0:?DWORD, % PCAP_FILE_TimeZone
        0:?DWORD, % PCAP_FILE_ActualTimestamp
        PCAP_FILE_SnapshotLength:?DWORD,
        PCAP_FILE_LinkType:?DWORD,
        PCAP_FILE_Body/binary
    >> = Content,
    io:format("PCAP_FILE_MajorVersion: ~p~n", [PCAP_FILE_MajorVersion]),
    io:format("PCAP_FILE_MinorVersion: ~p~n", [PCAP_FILE_MinorVersion]),
    io:format("PCAP_FILE_SnapshotLength: ~p~n", [PCAP_FILE_SnapshotLength]),
    io:format("PCAP_FILE_LinkType: ~p~n", [case PCAP_FILE_LinkType of
        0 -> "Loopback";
        1 -> "Ethernet";
        6 -> "Ring";
        7 -> "Arcnet";
        8 -> "Llip"
    end]),

    % unpack for pcap package header(http://www.manpagez.com/man/5/pcap-savefile/)
    io:format("~n=============================== PCAP PER-PACKET HEADER ===============================~n"),
    <<
        PCAP_Packet_Timestamp_Second:?DWORD,
        PCAP_Packet_Timestamp_MicroSecond:?DWORD,
        PCAP_Packet_Captured_Length:?DWORD,
        PCAP_Packet_Untruncated_Length:?DWORD,
        PCAP_Packet_Body/binary
    >> = PCAP_FILE_Body,
    io:format("PCAP_Packet_Timestamp_Second: ~p[~s]~n", [PCAP_Packet_Timestamp_Second, utils:format_timestamp(PCAP_Packet_Timestamp_Second)]),
    io:format("PCAP_Packet_Timestamp_MicroSecond: ~p~n", [PCAP_Packet_Timestamp_MicroSecond]),
    io:format("PCAP_Packet_Captured_Length: ~p~n", [PCAP_Packet_Captured_Length]),
    io:format("PCAP_Packet_Untruncated_Length: ~p~n", [PCAP_Packet_Untruncated_Length]),

    % Ether frame(https://en.wikipedia.org/wiki/Ethernet_frame#Ethernet_frame_types)
    io:format("~n=============================== Ethernet PACKET ===============================~n"),
    <<
        Ethernet_DstAddr:6/binary,
        Ethernet_SrcAddr:6/binary,
        Ethernet_Type:16/unsigned-big-integer,
        Ethernet_Body/binary
    >> = PCAP_Packet_Body,
    io:format("Ethernet_DstAddr: ~s~n", [utils:format_mac_addr(Ethernet_DstAddr)]),
    io:format("Ethernet_SrcAddr: ~s~n", [utils:format_mac_addr(Ethernet_SrcAddr)]),
    io:format("Ethernet_Type: ~4.16.0B ~p~n", [Ethernet_Type, utils:format_ethernet_proto_type(Ethernet_Type)]),


    % IP Fragment(https://tools.ietf.org/html/rfc791#page-11)
    io:format("~n=============================== IP PACKET ===============================~n"),
    <<
        IP_Version:4/unsigned-big-integer,
        IP_IHL:4/unsigned-big-integer,
        IP_TypeOfService:1/binary,
        IP_TotalLength:16/unsigned-big-integer,

        IP_Identification:16/unsigned-big-integer,
        IP_Flags:3/unsigned-big-integer,
        IP_FragmentOffset:13/unsigned-big-integer,

        IP_TimeToLive:8/unsigned-big-integer,
        IP_Protocol:8/unsigned-big-integer,
        IP_HeaderCheckSum:16/unsigned-big-integer,

        IP_SrcAddr:4/binary,
        IP_DstAddr:4/binary,

        IP_Remain/binary
    >> = Ethernet_Body,
    IP_Header_Len = IP_IHL * 4,
    IP_Options_Len = IP_Header_Len - 20,
    case IP_Options_Len of
        0 ->
            <<_:IP_Options_Len, IP_Body/binary>> = IP_Remain;
        _ ->
            IP_Body = IP_Remain
    end,
    io:format("IP_Version: ~p~n", [IP_Version]),
    io:format("IP_IHL: ~p; IP Header Length: ~p~n", [IP_IHL, IP_IHL * 4]),
    <<IP_TOS_Precedence:3, IP_TOS_Delay:1, IP_TOS_Throughput:1, IP_TOS_Relibility:1, 0:2>> = IP_TypeOfService,
    io:format("Service Quality:~n\tPrecedence: ~s~n\tDelay: ~s~n\tThroughput: ~s~n\tRelibility: ~s~n", [
            case IP_TOS_Precedence of
                2#111 -> "Network Control";
                2#110 -> "Internetwork Control";
                2#101 -> "CRITIC/ECP";
                2#100 -> "Flash Override";
                2#011 -> "Flash";
                2#010 -> "Immediate";
                2#001 -> "Priority";
                2#000 -> "Routine"
            end, case IP_TOS_Delay of
                0 -> "Normal";
                1 -> "Low"
            end, case IP_TOS_Throughput of
                0 -> "Normal";
                1 -> "Hight"
            end, case IP_TOS_Relibility of
                0 -> "Normal";
                1 -> "High"
            end
    ]),
    io:format("IP_TotalLength: ~p~n", [IP_TotalLength]),
    io:format("IP_Identification: 0X~.16B [~p]~n", [IP_Identification, IP_Identification]),
    io:format("IP_Flags: ~s~n", [case IP_Flags of
        0 -> "May Fragment && Last Fragment";
        1 -> "May Fragment && More Fragment";
        2 -> "Don't Fragment && Last Fragment";
        3 -> "Don't Fragment && More Fragment"
    end]),
    io:format("IP_FragmentOffset: ~p~n", [IP_FragmentOffset]),

    io:format("IP_TimeToLive: ~p~n", [IP_TimeToLive]),
    io:format("IP_Protocol: ~s~n", [utils:format_ip_proto_type(IP_Protocol)]),
    io:format("IP_HeaderCheckSum: 0X~.16B [~p]~n", [IP_HeaderCheckSum, IP_HeaderCheckSum]),

    io:format("IP_SrcAddr: ~s~n", [utils:format_ip_addr(IP_SrcAddr)]),
    io:format("IP_DstAddr: ~s~n", [utils:format_ip_addr(IP_DstAddr)]),

    % TCP Fragment(https://tools.ietf.org/html/rfc793#page-15)
    io:format("~n=============================== TCP PACKET ===============================~n"),
    <<
        TCP_SrcPort:16/unsigned-big-integer,
        TCP_DstPort:16/unsigned-big-integer,

        TCP_Sequence:32/unsigned-big-integer,
        TCP_ACKSequence:32/unsigned-big-integer,

        TCP_DataOffset:4,
        0:6, % TCP_Reserved mulst be zero [6 bit]
        TCP_IS_URG:1,
        TCP_IS_ACK:1,
        TCP_IS_PSH:1,
        TCP_IS_RST:1,
        TCP_IS_SYN:1,
        TCP_IS_FIN:1,
        TCP_Window:16/unsigned-big-integer,

        TCP_Checksum:16/unsigned-big-integer,
        TCP_UrgentPointer:16/unsigned-big-integer,

        TCP_Remain/binary
    >> = IP_Body,
    TCP_Header_Len = TCP_DataOffset * 4,
    TCP_Options_Len = TCP_DataOffset * 4 - 20,
    case TCP_Options_Len of
        0 ->
            <<_:TCP_Options_Len/binary, _/binary>> = TCP_Remain;
        _ ->
            _ = TCP_Remain
    end,
    io:format("TCP_SrcPort: ~p~n", [TCP_SrcPort]),
    io:format("TCP_DstPort: ~p~n", [TCP_DstPort]),
    io:format("TCP_Sequence: ~p [0x~8.16.0B]~n", [TCP_Sequence, TCP_Sequence]),
    io:format("TCP_ACKSequence: ~p [0x~8.16.0B]~n", [TCP_ACKSequence, TCP_ACKSequence]),
    io:format("TCP_DataOffset: ~p; TCP_Header_Len: ~p~n", [TCP_DataOffset, TCP_DataOffset * 4]),
    io:format("TCP Control Bits:~n"),
    if TCP_IS_URG == 1 -> io:format("\tURG:  Urgent Pointer field significant~n"); true -> null end,
    if TCP_IS_ACK == 1 -> io:format("\tACK:  Acknowledgment field significant~n"); true -> null end,
    if TCP_IS_PSH == 1 -> io:format("\tPSH:  Push Function~n"); true -> null end,
    if TCP_IS_RST == 1 -> io:format("\tRST:  Reset the connection~n"); true -> null end,
    if TCP_IS_SYN == 1 -> io:format("\tSYN:  Synchronize sequence numbers~n"); true -> null end,
    if TCP_IS_FIN == 1 -> io:format("\tFIN:  No more data from sender~n"); true -> null end,
    io:format("TCP_Window: ~p~n", [TCP_Window]),
    io:format("TCP_Checksum: ~p [0x~4.16.0B]~n", [TCP_Checksum, TCP_Checksum]),
    io:format("TCP_UrgentPointer: ~p [0x~4.16.0B]~n", [TCP_UrgentPointer, TCP_UrgentPointer]),

    io:format("~n=============================== TCP Body Skip ===============================~n"),
    TCP_Data_Len = IP_TotalLength - IP_Header_Len - TCP_Header_Len,
    io:format("TCP_Data_Len: ~p~n", [TCP_Data_Len]),

    {ok}.
