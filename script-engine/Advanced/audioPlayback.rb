require "audio-playback"

# Prompt the user to select an audio output
@output = AudioPlayback::Device::Output.gets

options = {
  :channels => [0,1],
  :latency => 1,
  :output_device => @output
}

@playback = AudioPlayback.play("test/media/1-stereo-44100.wav", options)

# Play in the foreground
@playback.block