#!/usr/bin/env tclsh

# Simple event driven modbus TCP server
# The server currently only handles holding and input register
# types but the basic proincipal can be applied to coil etc.

package require crc16

# Need to rewrite the GP (generating polynomial) to match modbus spec
# Lucky for us the internal lookup table is generated as needed
# that is when the CRC calc is requested the first time
# Original poly is:  or 0x8005
# set polynomial(crc16) [expr {(1<<16) | (1<<15) | (1<<2) | 1}]
# modbus GP is 0xA001 or [1<<15 | 1<<13 | 1]
# binary scan "A001" H* var
# set crc::polynomial(crc16) $var
# set crc::polynomial(crc16) [expr {(1<<15) | (1<<13) | 1}]
# changing poly breaks remote server
# set crc::polynomial(crc16) 0xA001

global holdingreg
global inputreg
global coil
global discrete

array set functext  [list \
                     1 "Read Coils" \
                     2 "Read Discrete inputs" \
                     3 "Read Holding register" \
                     4 "Read Input register" \
                     5 "Write single Coil" \
                     6 "Write single Holding register" \
                     7 "Read exception status" \
                     8 "Diagnostic serial" \
                     11 "Get Comm Event Counter" \
                     12 "Get Comm Event Log" \
                     15 "Write Multiple Coils" \
                     16 "Write multiple Holding registers" \
                     17 "Report Server ID" \
                     20 "Read File Record" \
                     21 "Write File Record" \
                     22 "Mask Write holding register" \
                     23 "ReadWrite Multiple Registers" \
                     24 "Read FIFO Queue" \
                     43 "Encapsulated Interface Transport" \
                     ]


# The func could be hard coded in the body of the call but that is something
# for later
array set funcjump [list \
                    1 {coil_1 $func $body} \
                    3 {holding3 $func $body} \
                    4 {input $func $body} \
                    5 {coil_5 $func $body} \
                    6 {holding6 $func $body} \
                    15 {coil_15 $func $body} \
                    16 {holding16 $func $body} \
                    22 {holding22 $func $body} \
                    ]

# Only need this for connection to serial to ethernet adapters
# RTU from a serial line presents a problem in that no initial
# length gets received as such some magic needs to be done up front
# to enable the reuse of the same procedures that are used by the TCP
# code
# rtulen is the body length as the header part can be either RTU or TCP
set rtulen(1) 6
# set rtulen(2) 6
set rtulen(3) 6
set rtulen(4) 6
set rtulen(5) 6
set rtulen(6) 6
set rtulen(7) 0

# after this things get complicated so the format needs to change
# messages from HEX 10 and up supply a variable length data field.
#
# When reading from a serial line we have no packet structure to do
# all the annoying packet stuff so here the length of the data field
# needs to be extracted from the serial input.
#
# The array now needs to include a sub header length with a marker
# to indicate which word is the length
set rtulen(16) [list 5 5]
set rtulen(22) 8

proc binaryPrint {data} {
    binary scan $data H* var
    puts $var
}

# coil data
# coils are 1 per array location???
proc coil_1 {func data} {
    global coilreg

    binary scan $data SS start len
    for {set i 0 } {$i < $len } {incr i} {
        lappend buffer $coilreg([expr $i + $start])
    }
    set blen [expr [llength $buffer] ]
    return [list [expr $blen + 2] [binary format ccb* $func $blen $buffer]]
}

proc coil_5 {func data} {
    global coilreg

    return {}
}

proc coil_15 {func data} {
    global coilreg

    return {}
}

proc input {func data} {
    global inputreg

    binary scan $data SS start len
    set end [expr $start + $len]
    for {set i $start} {$i < $end} {incr i} {
        lappend buffer $inputreg($i)
    }
    set blen [expr [llength $buffer] * 2]
    return [list [expr $blen + 2] [binary format ccS* $func $blen $buffer]]
}

proc holding3 {func data} {
    global holdingreg

    binary scan $data SS start len
    set end [expr $start + $len]
    for {set i $start} {$i < $end} {incr i} {
        lappend buffer $holdingreg($i)
    }
    set blen [expr [llength $buffer] * 2]
    return [list [expr $blen + 2] [binary format ccS* $func $blen $buffer]]
}

proc holding6 {func data} {
    global holdingreg

    binary scan $data SS addr value
    set reg $holdingreg($addr)
    set holdingreg($addr) $value
    lappend buffer $holdingreg($addr)

    if {$reg != $holdingreg($addr)} {
        set reg [format "%04X" $reg]
        set new [format "%04X" $holdingreg($addr)]
        puts "$reg > $new"
    }

    return [list 5 [binary format cSS $func $addr $holdingreg($addr)]]
}

proc holding16 {func data} {
    global holdingreg

    binary scan $data SScS* start len bc values
    for { set i 0 } {$i < $len} {incr i} {
        set pos [expr $start + $i]
        set holdingreg($pos) [lindex $values $i]
    }
    return [list 5 [binary format cSS $func $start $len]]
}

###########################################################################
# data is binary
# holdingreg is binary
proc holding22 {func data} {
    global holdingreg

    binary scan $data SSS addr andmask ormask
    set reg $holdingreg($addr)
    set holdingreg($addr) [expr ($reg & $andmask) | ($ormask & ~$andmask)]

    if {$reg != $holdingreg($addr)} {
        set reg [format "%04X" $reg]
        set new [format "%04X" $holdingreg($addr)]
        puts "$reg > $new"
    }
    # puts "reg = $holdingreg($addr)"

    return [list 7 [binary format cSSS $func $addr $andmask $ormask]]
}

###########################################################################
proc holding {func data} {
    global holdingreg

    if {$func == 3 } {
        binary scan $data SS start len
        # puts "Read holding registers"
        set end [expr $start + $len]
        for {set i $start} {$i < $end} {incr i} {
            lappend buffer $holdingreg($i)
        }
        set blen [expr [llength $buffer] * 2]
        return [list [expr $blen + 2] [binary format ccS* $func $blen $buffer]]
    } elseif {$func == 6} {
        binary scan $data SS addr value
        # puts "Write holding register $addr - $value"
        set holdingreg($addr) $value
        lappend buffer $holdingreg($addr)
        return [list 5 [binary format cSS $func $addr $holdingreg($addr)]]
    } elseif {$func == 16} {
        binary scan $data SScS* start len bc values
        # puts "Write multiple holding registers "
        for { set i 0 } {$i < $len} {incr i} {
            set pos [expr $start + $i]
            set holdingreg($pos) [lindex $values $i]
        }
        return [list 5 [binary format cSS $func $start $len]]
    } elseif {$func == 22} {
        # puts "Mask Write register"
        binary scan $data SSS addr andmask ormask
        binary scan $holdingreg($addr) H* var
        set $holdingreg($addr) [expr ($holdingreg($addr) & $andmask) | ($ormask & ~$andmask)]
        binary scan $holdingreg($addr) H* var2
        return [list 7 [binary format cSSS $func $addr $andmask $ormask]]
    }
    return {}
}

# This is here for the sake of brevity in the main block
proc clearEndpointData {channel} {
    global endpointData

    # This needs to be here
    if {[info exists endpointData($channel,timer)]} {
        after cancel $endpointData($channel,timer)
    }

    # Clear out everything I can't remember creating
    foreach v [array names endpointData "$channel,*"] {
        set endpointData($v) {}
    }

    # These need to exist otherwise they have to be surrounded with
    # exists tests in the code, it's simpler to create them here
    set endpointData($channel,head) {}
    set endpointData($channel,body) {}
    set endpointData($channel,txt) ""

    # The timer needs to be here as this is run after every packet
    # and after the socket is created
    set endpointData($channel,timer) [after 15000 [list dropSocket $channel]]
}

proc clearSerialEndpointData {channel} {
    global endpointData

    # Clear out everything I can't remember creating
    foreach v [array names endpointData "$channel,*"] {
        set endpointData($v) {}
    }

    # These need to exist otherwise they have to be surrounded with
    # exists tests in the code, it's simpler to create them here
    set endpointData($channel,head) {}
    set endpointData($channel,body) {}
}

#########################################################################
proc dropSocket {sock} {
    global endpointData

    puts "Dropping socket $sock"
    catch {close $sock}
    # cleanout any monitor timer
    if {[info exists endpointData($sock,timer)]} {
        catch {after cancel $endpointData($sock,timer)}
    }
    catch {array unset endpointData "$sock,*"}
    catch {unset endpointData($sock)}
}

##########################################################################
# endpointData
# endpointData managers the receive counters for a socket
# as the interface is in nonblock mode it can receive
# any number of bytes so state needs to be managed.
#
# The intent is to use the native TCL event loop and no other libraries
# to achieve this task.
#
# Appears to work ok
###########################################################################
proc tcpEventRead {sock} {
    global endpointData
    global functext
    global funcjump

    set myerror [fconfigure $sock -error]
    if {$myerror != ""} {
        dropSocket $sock
        return
    }

    # The timer is not necessary but it is expected any access to the server
    # to be frequent so a 5 second wait is appropriate after which the client
    # has probably died and this end needs to be cleaned up.
    #
    # This should probably be a config or command line option.
    if {[info exists endpointData($sock,timer)]} {
        catch {after cancel $endpointData($sock,timer)}
    }
    set endpointData($sock,timer) [after 15000 [list dropSocket $sock]]

    # Get the first 8 bytes, there is no pint getting anything less for this app
    # The MODTCP MBAP has a bunch of stuff that is never used here as this is not
    # a multi server setup.
    #
    # The text representation of the data is used for length as I currently can
    # not assure that
    set curlen [string length $endpointData($sock,head)]
    if {$curlen < 8} {
        set len [expr 8 - $curlen]
        set head [read $sock $len]
        if {$head == {}}  {
            dropSocket $sock
            return
        }
        # Append data to struct
        set endpointData($sock,head) $endpointData($sock,head)$head
        # binary scan $endpointData($sock,head) H* var
        # set endpointData($sock,txt) $var
        # puts "Read :$var"
    }

    # check if short packet as will need to go round the loop again
    set curlen [string length $endpointData($sock,head)]
    if {$curlen < 8} {
        return
    }

    # By now have enough info to start defining remainder of the packet
    binary scan $endpointData($sock,head) S2Scc mbap pktlen uid func

    # calcalate the remaining length
    set remlen [expr ($pktlen + 6) - $curlen]
    #sanity check, no currently handled packet can be less than the header
    if {$remlen <= 0} {
        puts "curlen :$curlen, pktlen:$pktlen"
        dropSocket $sock
        return
    }

    # append to data
    set data [read $sock $remlen]
    if {$data == {}}  {
        dropSocket $sock
        return
    }
    set endpointData($sock,body) $endpointData($sock,body)$data
    set datalen [string length $endpointData($sock,head)$endpointData($sock,body)]

    if { $datalen == [expr ($pktlen) + 6]} {
        binary scan $endpointData($sock,head)$endpointData($sock,body) H* var
        puts "$functext($func),Recv :$var"

        set body $endpointData($sock,body)
        if { [llength [set valuelist [eval $funcjump($func)] ]] != 0 } {
            set len [expr [lindex $valuelist 0] + 1]
            set data [binary format S2Sc $mbap $len $uid]

            # marshal up the data
            set txbuffer $data[lindex $valuelist 1]
            # puts -nonewline $sock $data[lindex $valuelist 1]
            puts -nonewline $sock $txbuffer
            flush $sock

            # binary scan $txbuffer H* var
            # puts "Sent :$var"
        }
        
        return 1
    } elseif { $datalen > [expr ($pktlen) + 6]} {
        puts "length error"
        dropSocket $sock
        return -1
    }
}

proc serialEventRTU {channel} {

    global endpointData

    if {[fblocked $channel]} {
		puts "Blocked $channel"
        return
    }

    #puts "Serial data on channel $channel"
    set ret [RTUEventRead $channel]
    switch $ret {
        -1 {
            # dropSocket $channel
            puts "Warning will robinson"
            exit 0
        }
        0 {

        }
        1 {
            clearSerialEndpointData $channel
        }
        default {
			puts "Alien attack"
		}
    }
    return
}

##########################################################################
# endpointData
# endpointData managers the receive counters for a socket
# as the interface is in nonblock mode it can receive
# any number of bytes so state needs to be managed.
#
# The intent is to use the native TCL event loop and no other libraries
# to achieve this task.
#
# Appears to work ok
# Three requests from modbus
# 01030000003c45db
# 010300c900321421
# 01030064004685e7
# 01160068fdfd00003793
###########################################################################
proc tcpRTUEventRead {sock} {
    global endpointData

    set myerror [fconfigure $sock -error]
    if {$myerror != ""} {
        dropSocket $sock
        return
    }

    # The timer is not necessary but it is expected any access to the server
    # to be frequent so a 5 second wait is appropriate after which the client
    # has probably died and this end needs to be cleaned up.
    #
    # This should probably be a config or command line option.
    if {[info exists endpointData($sock,timer)]} {
        catch {after cancel $endpointData($sock,timer)}
    }
    set endpointData($sock,timer) [after 15000 [list dropSocket $sock]]
    set ret [RTUEventRead $sock]
    switch $ret {
        -1 {
            dropSocket $sock
        }
        0 {

        }
        1 {
            clearEndpointData $sock
        }
        default {
		}
    }
}

proc RTUEventRead {sock} {
    global endpointData
    global holdingreg
    global rtulen
    global functext
    global funcjump

    # Get the first 2 bytes, there is no pint getting anything less for this app
    # The RTU over TCP has a bunch of stuff that is never used here as this is not
    # a multi server setup.
    #
    set curlen [string length $endpointData($sock,head)]
    if {$curlen < 2} {
        set len [expr 2 - $curlen]
        set head [read $sock $len]
        if {$head == {}}  {
            return -1
        }
        # Append data to struct
        set endpointData($sock,head) $endpointData($sock,head)$head
        # puts -nonewline "Read :"
        # binaryPrint $endpointData($sock,head)
        set curlen [string length $endpointData($sock,head)]
        set endpointData($sock,body) {}
    }

    # cheack we have all the head
    if {$curlen < 2} {
        return 0
    }
    #########################################################################

    # By now have enough info to start defining remainder of the packet
    binary scan $endpointData($sock,head) cc addr func
	# sanity check
    if {![info exists rtulen($func)]} {
        puts -nonewline ">>>>>>>>>>>>>> Invalid func:$func"
        binaryPrint $endpointData($sock,head)
        read $sock
        return 0
    }
    
    # calcalate the remaining length
    set curlen [string length $endpointData($sock,body)]
    # body length is defined in the RTULEN array
    if { [llength $rtulen($func)] == 1 } {
        set remlen [expr $rtulen($func) - $curlen]
        set endpointData($sock,maxlen) [expr $rtulen($func)+2]
    } else {
		# when we get here we already have 2 bytes from the packet
		# curlen is the body after these two bytes
		if { $func == 16 } {
			# puts "Processing $functext($func)"
			# Head 0010
			# Body 0001,0008,10,000a,000a,000a,000a,000a,000a,000a,000a
			set hl [lindex $rtulen($func) 0]
			set bl [lindex $rtulen($func) 1]
			if { $hl > $curlen } {
				set remlen [expr $hl - $curlen]
				set data [read $sock $remlen]
				if {$data != {}}  {
					set endpointData($sock,body) $endpointData($sock,body)$data
				}
				return 0
			} else {
				# (hl - 2) is less than or equal to curlen in this
				# instance 5 extract length data from body
				binary scan $endpointData($sock,body) SSc start regcount bc
				# remember to add 2 for the crc
				set endpointData($sock,maxlen) [expr $hl + 2 + $bc + 2]
			}
		}
    }

	# data is read in bytes so use bc as it represents the 
	# total tail bytes to be read
	set remlen [expr ($endpointData($sock,maxlen) -2) - $curlen]
    #sanity check, no currently handled packet can be less than the header
    if {$remlen <= 0} {
		puts "Bad remlen $remlen"
		binaryPrint $endpointData($sock,body)
        return -1
    }

    # append to data
    set data [read $sock $remlen]
    if {$data == {}}  {
		# puts "Empty read??"
        return 0
    } else {
		# Archive data
		set endpointData($sock,body) $endpointData($sock,body)$data
		
		# See if we need to go back for more
		if { [string length $endpointData($sock,body)] < $remlen } {
			# puts "not enough [string length $endpointData($sock,body)]:$remlen"
			return 0
		}
	}
	
    set datalen [string length $endpointData($sock,head)$endpointData($sock,body)]
    if { $datalen == $endpointData($sock,maxlen)} {
        # debug
        # binary scan $endpointData($sock,head)$endpointData($sock,body) H* var
        if { $func != 22 } {
            puts -nonewline "$functext($func),Recv :"
            binaryPrint $endpointData($sock,head)$endpointData($sock,body)
        }

        set body $endpointData($sock,body)
        if { [llength [set valuelist [eval $funcjump($func)] ]] != 0 } {
            set data [binary format c $addr]
            set mycrc [crc::crc16 -format %04X -seed 0xFFFF $data[lindex $valuelist 1]]

            # I could do the following in 1 line but I want to make
            # it clear that these get swapped
            set high [string range $mycrc 0 1]
            set low [string range $mycrc 2 end]
            # re-arrange mycrc
            set mycrc $low$high

            # marshal up all the data and send
            set txbuffer $data[lindex $valuelist 1][binary format H* $mycrc]
            puts -nonewline $sock $txbuffer
            flush $sock

            # debug out
            # puts -nonewline "Sent :"
            # binaryPrint $txbuffer

        }
        ##################################################################
        # this in a special for an application where holding reg 0 is used
        # as a heartbeat to determine health
        incr holdingreg(0)
        if { $holdingreg(0) > 0xFFFF } {
            set holdingreg(0) 0
        }
        
        ##################################################################
        return 1
        
    } elseif { $datalen > [expr $endpointData($sock,maxlen) + 2]} {
        puts "length error $datalen:$curlen"
        return -1
    }

    return 0
}

proc tcpCheck {channel} {
    set myerror [fconfigure $channel -error]
    puts "tcpCheck $channel $myerror"
    if { $myerror != ""} {
        puts $myerror
        dropSocket $channel
        exit
    }
}

# basic server socket connect thing
proc rtuConnect {channel clientaddr clientport} {
    global holdingreg
    global endpointData
    set str "[clock format [clock seconds] -format {%H:%M:%S}] - Connecting from $clientaddr $clientport, $channel"
    puts $str
    fconfigure $channel -translation binary
    fconfigure $channel -blocking 0
    fileevent $channel readable [list tcpRTUEventRead $channel]
    clearEndpointData $channel
}

# basic server socket connect thing
proc modtcpConnect {channel clientaddr clientport} {
    global holdingreg
    global endpointData
    set str "[clock format [clock seconds] -format {%H:%M:%S}] - Connecting from $clientaddr $clientport, $channel"
    puts $str
    # exec logger -p "local3.info" -t modbustcl $str
    fconfigure $channel -translation binary
    fconfigure $channel -blocking 0
    fileevent $channel readable [list tcpEventRead $channel]
    clearEndpointData $channel
}

proc showHelp {} {
    global funcjump
    global functext
    puts "Help stuff"
    puts "usage bintest.tcl <register size> <port>"
    puts "The default register size is 512"
    puts "The default port is 5020"
    puts "No help really as this is just a simple holding reg TCP modbus implementation"
    puts ""
    puts "--------------------------------------------------------------------------"
    puts "Supported codes [join [lsort -integer -increasing [array names funcjump]] {,}]"
    foreach name [lsort -integer -increasing [array names funcjump]] {
        puts "$name - $functext($name)"
    }
    puts "--------------------------------------------------------------------------"
    puts ""
}

showHelp

set depth 512
if {$::argc > 0 } {
    set depth [lindex $::argv 0]
}

set port 5020
if {$::argc > 1 } {
    set port [lindex $::argv 1]
}

puts "Creating $depth holding registers"
for {set i 0} {$i < $depth} {incr i} {
    set holdingreg($i) 257
    # [binary format H* "0101"]
    set inputreg($i) 257
    set coilreg($i) 0
}

# initialise heartbeat counter
set holdingreg(0) 0

set filename /var/log/testfifo
set host localhost

puts "Starting server socket"
socket -server rtuConnect $port
puts "Starting server socket"
#socket -server modtcpConnect 502

puts "Starting serial port"
if {$tcl_platform(os) == {Linux} } {
    set device "/dev/ttyUSB0"
    # set fh [open {/dev/ttyUSB0} RDWR]
	set fh [open $device {RDWR NONBLOCK}]
	exec stty -F $device clocal
} else {
    set fh [open {//./COM4} RDWR]
}

fconfigure $fh -blocking 0 -translation binary -buffering none -eofchar {}
fileevent $fh readable [list serialEventRTU $fh]
clearSerialEndpointData $fh
puts "Serial port connected $fh"

proc showStats {fh} {
    global holdingreg
    puts "[clock format [clock seconds] -format {%H:%M:%S}] - counter : $holdingreg(0)"
    # puts "Serial handle $fh"
    after 10000 [list showStats $fh]
}
showStats $fh

puts ">>>>>>>>>>>>>>>>>>>> Waiting forever <<<<<<<<<<<<<<<<<"
vwait forever
