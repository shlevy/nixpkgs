import ./. {
  crossSystem = {
    config = "arm-apple-darwin10";

    useiOSCross = true;

    arch = "armv7";

    libc = "libSystem";
  };

/*  config.packageOverrides = p: {
#    llvmPackages = p.llvmPackages_3_8;
  };*/
}
