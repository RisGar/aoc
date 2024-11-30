let read_lines filename =
  In_channel.with_open_text ("inputs/" ^ filename) In_channel.input_lines
