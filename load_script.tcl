# Script mostly from:
# http://quartushelp.altera.com/current/mergedProjects/tafs/tafs/tcl_pkg_jtag_ver_1.0_cmd_get_device_names.htm
#
# List all available programming hardwares, and select the USBBlaster.
# (Note: this example assumes only one USBBlaster connected.)
#puts "JTAG chains:"

if { [catch get_hardware_names] } {
	puts "ERROR: USB-Blaster hardware not found."
	exit
}

foreach hardware_name [get_hardware_names] {
	#puts $hardware_name
	if { [string match "USB-Blaster*" $hardware_name] } {
		set usbblaster_name $hardware_name
	}
	if { [string match "DE-SoC*" $hardware_name] } {
		set usbblaster_name $hardware_name
	}
}

# List all devices on the chain, and select the first device on the chain.
#puts "\nDevices available on JTAG chain:"
set target_device "0"
foreach device_name [get_device_names -hardware_name $usbblaster_name] {
	#puts $device_name
	if { [string match "@1*" $device_name] } {
		set target_device $device_name
	}
}

# The DE1-SoC's FPGA is device number 2
if { [string match "@1: SOCVHPS*" $target_device] } {
	set target_device "0"
	foreach device_name [get_device_names -hardware_name $usbblaster_name] {
		# puts $device_name
		if { [string match "@2*" $device_name] } {
			set target_device $device_name
		}
	}
}

if {$target_device==0} {
	puts "ERROR: Target device not found."
	exit
}

puts "\nConnecting to $usbblaster_name $target_device";

set lastbyte "0"
set mem_list [get_editable_mem_instances -hardware_name $usbblaster_name -device_name $target_device]
foreach i $mem_list {
	# puts "[lindex $i 0],[lindex $i 1],[lindex $i 2],[lindex $i 3],[lindex $i 4],[lindex $i 5]"
	if {"CODE"==[lindex $i 5]} {
		# puts "Valid memory found"
		set myindex [lindex $i 0]
		set lastbyte [expr [lindex $i 1]-1]
	}
}
# puts "myindex=$myindex, mysize=$lastbyte"

if {$lastbyte==0} {
	puts "ERROR: 'CODE' memory not found in target device."
	exit
}

begin_memory_edit -hardware_name $usbblaster_name -device_name $target_device
puts "Sending HEX file '[lindex $argv 0]'..."
update_content_to_memory_from_file -instance_index $myindex -mem_file_path "[lindex $argv 0]" -mem_file_type hex
puts "Sending command to copy hex file to 'flash' memory..."
write_content_to_memory -instance_index $myindex -start_address $lastbyte -word_count 1 -content "55" -content_in_hex
end_memory_edit
puts "Done."
exit
