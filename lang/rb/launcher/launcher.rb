#!/usr/bin/env ruby

class Launcher

	def initialize(app_map)
		@app_map = app_map
	end

	def run(file_name)
		application = select_app(file_name)
		system("#{application} #{file_name}")
	end

	def select_app(file_name)
		ftype = file_type(file_name)
		@app_map[ftype]
	end

	def file_type(file_name)
		File.extname(file_name).gsub( /^\./, '' ).downcase
	end

	def help
		print "
		Please pass in the path of the file to launch.

		Usage: #{__FILE__} target_file
		"
	end

	if ARGV.empty?
		help
		exit
	else
		app_map = {
			'html' => 'qutebrowser',
			'rb' => 'nvim',
			'js' => 'nvim',
			'jpg' => 'viewnior',
			'png' => 'viewnior'
		}

		l = Launcher.new(app_map)
		target = ARGV.join(' ')
		l.run(target)
	end
end

