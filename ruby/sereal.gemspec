
Gem::Specification.new do |s|
  s.name         = "sereal"
  s.version      = "0.0.17"
  s.summary      = "Sereal encoder/decoder"
  s.description  = "Sereal encoder/decoder (https://github.com/Sereal/Sereal)"
  s.homepage     = "https://github.com/Sereal/Sereal"
  s.authors      = ["Borislav Nikolov"]
  s.email        = "jack@sofialondonmoskva.com"
  s.files        = Dir.glob(File.join("ext","**","*.{c,h,rb}"))
  s.has_rdoc     = true
  s.extensions   << File.join("ext","sereal","extconf.rb")
  s.executables  << 'rsrl'
  s.rdoc_options = %w[ --exclude .*\.so ]
  s.extra_rdoc_files = [ File.join("ext","sereal","sereal.c") ]
  s.add_development_dependency "rake-compiler"
end
