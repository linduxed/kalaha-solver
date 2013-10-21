guard 'shell' do
  watch(%r{^Kalaha/(.*)\.hs$}) { |m| `runghc spec/#{m[1]}Spec.hs` }
  watch(%r{^spec/(.*)$}) { |m| `runghc #{m[0]}` }
end
