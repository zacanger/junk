#!/usr/bin/env ruby

require 'launcher'

def help
	print "
	Please pass in the path of the file to launch.

	Usage: #{__FILE__} target_file
	"
end

unless ARGV.size > 0
	help
	exit
else
	app_map = {
		'html' => 'qutebrowser',
		'js' => 'nvim',
		'rb' => 'nvim',
		'md' => 'nvim',
		'jpg' => 'viewnior',
		'png' => 'viewnior'
	}

	l = Launcher.new(app_map)
	target = ARGV.join(' ')
	l.run(target)
end

