
Gem::Specification.new do |s|
  s.name    = "sereal"
  s.version = "0.0.1"
  s.summary = "Sereal encoder/decoder for ruby (https://github.com/Sereal/Sereal)"
  s.author  = "Borislav Nikolov"
  s.email   = "jack@sofialondonmoskva.com"
  s.files = Dir.glob("ext/**/*.{c,h,rb}") +
            Dir.glob("lib/**/*.rb")
  
  s.extensions << "ext/sereal/extconf.rb"
  
  s.add_development_dependency "rake-compiler"
end
