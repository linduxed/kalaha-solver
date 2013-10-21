guard 'shell' do
  watch(%r{^Kalaha/(.*)\.hs$}) { |m| system("runghc spec/#{m[1]}Spec.hs" )}
  watch(%r{^spec/(.*)$}) { |m| system("runghc #{m[0]}") }
end
