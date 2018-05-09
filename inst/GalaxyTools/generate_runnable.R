# - - - - - - - - - - - place function and define fun_name above this line -  - - - - - - - - -
#print(paste0('Calling ',fun_name))

# Generate Options

suppressMessages(require(optparse))
option_list = lapply(names(formals(fun_name)),FUN=function(arg){

    arg_split   = unlist(strsplit(arg,split = '_'))
    arg_short   = paste('-', ifelse(length(arg_split)>1,
                            paste0(sapply(arg_split,FUN=function(x) substring(x, 1, 1)),collapse=''), # TRUE
                            substring(arg, 1, 1)),sep='')                                             # DEFAULT
    arg_long    = paste0('--',arg,sep='')

    make_option(c(arg_long) )
})

# Parse
opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

# Extract arguments
args        = lapply(names(opt),FUN=function(x) opt[[x]] )
names(args) = names(opt)

# Execute function
args[['help']] = NULL
do.call(fun_name, c(args))
