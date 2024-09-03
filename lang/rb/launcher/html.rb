class Html

	DEFAULT_BROWSER = 'qutebrowser'

	def run file, args
		if args.empty?
			system("#{DEFAULT_BROWSER} #{FILE}")
		else
			dispatch_on_parameters file, args
		end
	end

	def dispatch_on_parameters file, args
		cmd = args.shift
		send("do_#{cmd}", file, args)
	end

	def do_chromium file, args=nil
		system("chromium #{file} #{args}")
	end

	def do_lynx file, args=nil
		system("lynx #{file} #{args}")
	end

	def do_report(file, args=nil)
		require'rexml/document'
		begin
			dom = REXML::Document.new(IO.read(file))
			if args.empty?
				puts basic_xhtml_report(dom)
			else
				puts report_on(dom, args.first)
			end
		rescue Exception
			warn "Problem reading '#{file}':\n#{$!}"
		end
	end

	def report_on dom, element
		els = dom.root.elements.to_a("//#{element}")
		"document has #{els.size} '#{element}' elements"
	end

	def basic_xhtml_report(dom)
		report = []
		css = dom.root.elements.to_a('//link[@rel="stylesheet"]')
		unless css.empty?
			report << "file references #{css.size} stylesheets"
			css.each do |el|
				file_name = el.attributes['href']
				file_name.gsub!( /^\//, '')
				unless File.exist?(file_name)
					report << "*** cannot find '#{file_name}'"
				end
			end
		end

		js = dom.root.elements.to_a('//script')
		unless js.empty?
			report << "file references #{js.size} js files"
			js.each do |el|
				file_name = el.attributes['src']
				file_name.gsub!( /^\//, '')
				unless File.exist?(file_name)
					report << "*** cannot find '#{file_name}'"
				end
			end
		end

		report.join("\n")
	end

end

