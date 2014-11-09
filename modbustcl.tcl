#!/usr/bin/env tclsh

global holdingreg



proc readNetRTU {channel} {
    global holdingreg

    set data [read -nonewline $channel]
    binary scan $data ccSSS addr func start len crc
    puts "addr:$addr, func:$func $start $len"
    set end [expr $start + $len]
    for {set i $start} {$i < $end} {incr i} {
        if {[info exists holdingreg($i)]} {
            set buffer $buffer$holdingreg($i)
        } else {
            set buffer $buffer\x0000
        }
    }
    write $channel [binary format cccS* $addr $func $buffer]
    flush $channel
}

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
        puts "Read holding registers"
        set end [expr $start + $len]
        for {set i $start} {$i < $end} {incr i} {
            lappend buffer $inputreg($i)
        }
        set blen [expr [llength $buffer] * 2]
        # set data [binary format S2ScccS* $mbap [expr $blen + 3] $uid $func $blen $buffer]
        return [list [expr $blen + 2] [binary format ccS* $func $blen $buffer]]
    }
    return {}
}

proc holding {func data} {
    global holdingreg

    if {$func == 3 } {
        # Read remainder of frame remember to subtract 1 for UID
        binary scan $data SS start len
        puts "Read holding registers"
        set end [expr $start + $len]
        for {set i $start} {$i < $end} {incr i} {
            lappend buffer $holdingreg($i)
        }
        set blen [expr [llength $buffer] * 2]
        # set data [binary format S2ScccS* $mbap [expr $blen + 3] $uid $func $blen $buffer]
        return [list [expr $blen + 2] [binary format ccS* $func $blen $buffer]]
    } elseif {$func == 6} {
        # Read remainder of frame remember to subtract 1 for UID
        binary scan $data SS addr value
        #puts " $addr $value"
        puts "Write holding register $addr - $value"
        set holdingreg($addr) $value
        lappend buffer $holdingreg($addr)
        #set data [binary format S2SccSS $mbap 6 $uid $func $addr $holdingreg($addr)]
        return [list 5 [binary format cSS $func $addr $holdingreg($addr)]]
    } elseif {$func == 16} {
        binary scan $data SScS* start len bc values
        puts "Write multiple holding registers "
        for { set i 0 } {$i < $len} {incr i} {
            set pos [expr $start + $i]
            set holdingreg($pos) [lindex $values $i]
        }
        # set data [binary format S2SccSS $mbap 6 $uid $func $start $len]
        return [list 5 [binary format cSS $func $start $len]]
    } elseif {$func == 22} {
        binary scan $data SSS addr andmask ormask
        set $holdingreg($addr) [expr ($holdingreg($addr) & andmask) | (ormask & ~andmask)]
        # set data [binary format S2SccSSS $mbap 8 $uid $func $addr $andmask $ormask]
        return [list 7 [binary format cSSS $func $addr $andmask $ormask]]
    }
    return {}
}

proc readNetTCP {channel} {
    # Read the MBAP header including UID
    set head [read $channel 8]
    if { $head == "" } {
        puts "Empty head"
        close $channel
        return
    }
    binary scan $head S2Scc mbap pktlen uid func
    set body [read $channel [expr $pktlen - 2]]
    puts "mbap:$mbap, PKTLEN:$pktlen UID:$uid func:$func"

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
        #binary scan $data[lindex $valuelist 1] H* var
        #puts "Sent :$var"
    }
}

proc connect {channel clientaddr clientport} {
    puts "Connecting"
    fconfigure $channel -translation binary
    fconfigure $channel -blocking 0
    fileevent $channel readable [list readNetTCP $channel]
}

proc openNet {host port} {
    set fh [socket -server connect $port]
    return $fh
}

set count 1
proc readFifo {handle} {
    global count
    puts "Reading from fifo"
    gets $handle str
    puts $str
    if {$count > 5} {
        exit 0
    }
    incr count
}

proc openFifo {filename} {
    puts "Opeing fifo"
    set fh [open $filename [list RDONLY]]
    # fconfigure $fh -blocking 0
    fileevent $fh readable [list readFifo $fh]
    puts "Fifo configured"
}

proc showHelp {} {
    puts "Help stuff"
    puts "usage bintest.tcl <register size>"
    puts "The default register size is 512"
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
puts "Creating $depth holding registers"
for {set i 0} {$i < $depth} {incr i} {
    set holdingreg($i) 0
}

set filename /var/log/testfifo
set host localhost
set port 5020

puts "Starting server socket"
socket -server connect $port
# set server [openNet $host $port]

# set fifo [openFifo $filename]
puts "Waiting forever"
vwait forever
