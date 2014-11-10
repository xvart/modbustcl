#!/usr/bin/env tclsh

# Simple event driven modbus TCP server
# The server currently only handles holding and input register
# types but the basic proincipal can be applied to coil etc.

# It has no error handling but this is trivial to add someone
# needs to write the code

global holdingreg
global inputreg

proc coil {func data} {
    global holdingreg
    if { $func == 1 } {
        binary scan $data SS start len
        for {set i 0 } {i < [expr $len/8] } {incr i} {
            lappend buffer 0
        }
        set blen [expr [llength $buffer] ]
        return [list [expr $blen + 2] [binary format ccc* $func $blen $buffer]]
    } elseif { $func == 5 } {
    } elseif { $func == 15 } {
    }
    return {}
}

proc input {func data} {
    global inputreg

    if {$func == 4 } {
        # Read remainder of frame remember to subtract 1 for UID
        binary scan $data SS start len
        puts "Read input registers"
        set end [expr $start + $len]
        for {set i $start} {$i < $end} {incr i} {
            lappend buffer $inputreg($i)
        }
        set blen [expr [llength $buffer] * 2]
        return [list [expr $blen + 2] [binary format ccS* $func $blen $buffer]]
    }
    return {}
}

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
        puts "Mask Write register"
        binary scan $data SSS addr andmask ormask
        binary scan $holdingreg($addr) H* var
        set $holdingreg($addr) [expr ($holdingreg($addr) & $andmask) | ($ormask & ~$andmask)]
        binary scan $holdingreg($addr) H* var2
        puts "$var:$var2"

        return [list 7 [binary format cSSS $func $addr $andmask $ormask]]
    }
    return {}
}

proc dropSocket {sock} {
    global socketdata
    close $sock
    array unset socketdata "$sock,*"
    unset socketdata($sock)
}




##########################################################################
# socketdata
# socketdata managers the receive counters for a socket
# as the interface is in nonblock mode it can receive
# any number of bytes so state needs to be managed.
#
# The intent is to use the native TCL event loop and no other libraries
# to achieve this task.
###########################################################################
proc tcpEventRead {$sock} {
    global socketdata

    catch{after cancel $socketdata($sock,timer)}
    set socketdata($sock,timer) [after 5000 [list dropSocket $sock]]

    # Get the first 8 bytes
    if {[string length $socketdata($sock,txt)] < 16} {
        set len [expr 8 - ([string length $socketdata($sock,txt)] / 2)]
        set head [read $channel $len]

        # Append data to struct
        set socketdata($sock,head) $socketdata($sock,head)$head
        binary scan $socketdata($sock,head) H* var
        set socketdata($sock,txt) $var
        set socketdata($sock,body) {}
    }

    # check if short packet as will need to go round the loop again
    set curlen [string length $socketdata($sock,txt)]
    if {$curlen < 16} {
        return
    }

    # By now have enough info to start defining remainder of the packet
    binary scan $socketdata($sock,head) S2Scc mbap pktlen uid func

    # calcalate the remaining length
    set remlen [expr ($pktlen + 14) - $curlen]

    # append red to data
    set socketdata($sock,body) $socketdata($sock,body)[read $channel $remlen]
    binary scan $socketdata($sock,head)$socketdata($sock,body) H* var

    if { [string length $var] == [expr $pktlen + 14]} {
        set body $socketdata($sock,body)
        array unset socketdata "$sock,*"
        if { [llength [set valuelist [holding $func $body]]] == 0 } {
            if { [llength [set valuelist [input $func $body]]] == 0 } {
            #    if { [coil  $func $body] != true} {
            #        discrete  $func $body
            #    }
            }
        }
    }

    if { [llength $valuelist] > 0 } {
        set len [expr [lindex $valuelist 0] + 1]
        set data [binary format S2Sc $mbap $len $uid]
        puts -nonewline $sock $data[lindex $valuelist 1]
        flush $sock

        binary scan $data[lindex $valuelist 1] H* var
        puts "Sent :$var"
    }
}



#########################################################################
# This is effective for reads when packets arrive complete.
# It has issues when partial packets are received.
#########################################################################

proc readNetTCP {channel} {
    # Read the MBAP header including UID
    set head {}
    set head [read $channel 8]
    binary scan $head H* var

    if { $head == "" } {
        # puts "Empty head"
        close $channel
        return
    }
    binary scan $head S2Scc mbap pktlen uid func
    set body [read $channel [expr $pktlen - 2]]
    puts "mbap:$mbap, PKTLEN:$pktlen UID:$uid func:$func"
    binary scan $body H* bvar
    puts "Read :$var$bvar"

    if { [llength [set valuelist [holding $func $body]]] == 0 } {
        if { [llength [set valuelist [input $func $body]]] == 0 } {
        #    if { [coil  $func $body] != true} {
        #        discrete  $func $body
        #    }
        }
    }

    if { [llength $valuelist] > 0 } {
        set len [expr [lindex $valuelist 0] + 1]
        set data [binary format S2Sc $mbap $len $uid]
        puts -nonewline $channel $data[lindex $valuelist 1]
        flush $channel

        binary scan $data[lindex $valuelist 1] H* var
        puts "Sent :$var"
    }
}

# Only need this for connection to serial to ethernet adapters
# RTU from a serial line presents a problem in that no initial
# length gets received as such some magic needs to be done up front
# to enable the reuse of the same procedures that are used by the TCP
# code
set rtulen(0) {binary ccSSS* $data}
set rtulen(1) 4
set rtulen(2) 4
set rtulen(3) 4
set rtulen(4) 4
set rtulen(5) 4
set rtulen(6) 4
set rtulen(7) 0
set rtulen(11) 0
set rtulen(12) 0
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
set rtulen(3) 4
set rtulen(3) 4
set rtulen(3) 4
proc readRTU {channel} {
    global holdingreg

    set head [read $channel 2]
    binary scan $data cc addr func
    if {[info exists rtulen($func)]} {
        set body [read $channel 2]
    } else {
        return
    }

    # puts "addr:$addr, func:$func $start $len"
    set end [expr $start + $len]
    if { [llength [set valuelist [holding $func $body]]] == 0 } {
        if { [llength [set valuelist [input $func $body]]] == 0 } {
        #    if { [coil  $func $body] != true} {
        #        discrete  $func $body
        #    }
        }
    }
    if { [llength $valuelist] > 0 } {
        set len [expr [lindex $valuelist 0] + 1]
        set data [binary format cc $addr $func]
        puts -nonewline $channel $data[lindex $valuelist 1]
        flush $channel

        #binary scan $data[lindex $valuelist 1] H* var
        #puts "Sent :$var"
    }
    flush $channel
}

# basic server socket connect thing
proc connect {channel clientaddr clientport} {
    global socketdata
    puts "Connecting from $clientaddr $clientport"
    fconfigure $channel -translation binary
    fconfigure $channel -blocking 0
    # fileevent $channel readable [list readNetTCP $channel]
    fileevent $channel readable [list tcpEventRead $channel]
    set socketdata($channel) $channel
    set socketdata($channel,rcv) {}
    set socketdata($channel,txt) ""
}

proc showHelp {} {
    puts "Help stuff"
    puts "usage bintest.tcl <register size> <port>"
    puts "The default register size is 512"
    puts "The default port is 5020"
    puts "No help really as this is just a simple holding reg TCP modbus implementation"
    puts "--------------------------------------------------------------------------"
    puts "Supported codes are 3,6,16 and 22"
    puts "3 - Read reagister"
    puts "6 - Write register"
    puts "16 - Write multiple registers"
    puts "22 - Mask write register"
    puts "--------------------------------------------------------------------------"
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
    set holdingreg($i) 0
    set inputreg($i) 0
}

set filename /var/log/testfifo
set host localhost

puts "Starting server socket"
socket -server connect $port

puts "Waiting forever"
vwait forever
