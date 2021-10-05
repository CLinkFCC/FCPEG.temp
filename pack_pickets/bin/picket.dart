void main(List<String> arguments) {
  Commander cdr = Commander.bind();
  //subcommands: init, create, delete, move, list, serve, publish, check, poll help, version
  cdr.listen(arguments);

}


