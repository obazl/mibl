#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <glob.h>
#include <libgen.h>
#ifdef LINUX                    /* FIXME */
#include <linux/limits.h>
#else // FIXME: macos test
#include <limits.h>             /* PATH_MAX */
#endif
#include <pwd.h>
#include <spawn.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/stat.h>

#include "ini.h"
#include "log.h"
#if EXPORT_INTERFACE

#include "utarray.h"
#include "utstring.h"
#endif

#include "bazel_config.h"
#include "deploy_config.h"
#include "oibl_config.h"
#include "fs.h"

UT_string *xdg_home_install_root; // $HOME/.local/share/libs7
UT_string *xdg_install_dir;
UT_string *xdg_home_bin;

//FIXME: use #ifdef DEBUG_TRACE instead of if(trace)

EXPORT void deploy(void)
{
    if (trace)
        log_debug("deploy_configure");

    /* FIXME: s7_config detects manifest too */
    char *mdir = dirname(utstring_body(exec_root)); // _wd);
    UT_string *manifest;
    utstring_new(manifest);
    utstring_printf(manifest, "%s%s", mdir, "/MANIFEST");
    int rc = access(utstring_body(manifest), R_OK);
    if (rc) {
        if (verbose || debug)
            log_info("MANIFEST not found: %s",
                     utstring_body(manifest));
    } else {
        if (verbose)
            log_debug("FOUND: %s",
                     utstring_body(manifest));
    }

    _config_xdg_sys_load_paths();
    _config_xdg_home_load_paths();
    _deploy_scm_files(manifest);
}

/*
  sets bazel_script_dir to dir containing scriptfile, which must be
  passed in 'data' attrib of cc_binary rule
 */
LOCAL void _deploy_scm_files(UT_string *manifest)
{
    FILE * fp;
    char * line = NULL;
    size_t len = 0;
    ssize_t read_ct;
    int rc = 0;

    /* are we running in dev mode (launched from oibl proj root) or
       as an external import? */
    if (utstring_len(ws_root) != 0) {
        /* bazel_configure detected BUILD_WORKSPACE_DIRECTORY */

        if (trace)
            log_debug("WS ROOT: %s", utstring_body(ws_root));
        UT_string *ws_file;
        utstring_new(ws_file);
        utstring_printf(ws_file,
                        "%s/WORKSPACE.bazel", utstring_body(ws_root));
        rc = access(utstring_body(ws_file), R_OK);
        if (rc) {
            if (verbose || debug)
                log_info("WS file not found: %s",
                         utstring_body(ws_file));
        } else {
            if (verbose)
                log_debug("WS file FOUND: %s",
                          utstring_body(ws_file));
            FILE *wsfile;
            char fileText [100];
            wsfile = fopen(utstring_body(ws_file), "r");
            fgets(fileText, 100, wsfile);
            char *r = strstr(fileText,
                             "workspace(name = \"oibl\")");
            if (r != NULL) {
                dev_mode = true;
                if (trace)
                    log_debug("WS name: oibl");
                fclose(wsfile);
            }
            fclose(wsfile);
        }
    }
    if (debug)
        log_debug("dev mode? %d", dev_mode);

    /*
      build scripts list their scm srcs in the 'data' attrib, which
      puts them in the runfiles area. they are listed in MANIFEST, so
      we need to parse it to find out what to copy where.
     */

    fp = fopen(utstring_body(manifest), "r");
    if (fp == NULL) {
        log_error("MANIFEST fopen failure %s", utstring_body(manifest));
        /* exit(EXIT_FAILURE); */
    }

    if (debug)
        log_debug("Reading MANIFEST");

    char *oibl_libs7 = NULL;

    while ((read_ct = getline(&line, &len, fp)) != -1) {
        /* log_debug("Retrieved line of length %zu:", read_ct); */

        line[strcspn(line, "\n")] = '\0';    /* trim trailing newline */
        /* log_debug("%s", line); */

        /* two tokens per line, first is path relative to exec dir,
           second is corresponding absolute path */

        /* 1st token: get xdg subdir by removing pre __main__/libs7 or
           libs7*/

        /* 2nd token: file to copy to xdg */

        char *token, *sep = " ";
        char *dest;
        token = strtok((char*)line, sep);
        dest = token;
        if (trace)
            log_debug("dest: %s", dest);

        if (token != NULL) {
            token = strtok(NULL, sep);
        } else {
            /* log_debug("skipping entry"); */
            continue;
        }

        /* log_debug("bn: %s", basename(token)); */
        if ( (strncmp(basename(token),
                      "libc_s7.so", 10) == 0) ) {
            if (trace)
                log_info("FOUND LIBC_S7.SO: %s", dest);
            _copy_file(token, utstring_body(xdg_home_install_root));
            continue;
        }
        if ( (strncmp(basename(token),
                      "repl", 4) == 0) ) {
            char *bn = basename(token);
            if (strlen(bn) == 4) {
                if (trace)
                    log_info("FOUND REPL tok: %s", basename(token));
                _copy_file(token, utstring_body(xdg_home_bin));
                continue;
            }
        }

        char *ext = strrchr(token, '.');
        if (ext != NULL) {
            if ( (strncmp(ext, ".scm", 4) == 0) && strlen(ext) == 4) {
                /* log_info("src: %s", token); */

                char *destdir = dirname(dest);

                /*
                  if oibl is imported as an external repo we get
                  lines like:
                  __main__/external/oibl/libs7/oibl/dune.scm
                  __main__/external/oibl/libs7/oibl/dune_actions.scm

                  otherwise, in dev mode (launch from projroot) we get:
                  __main__/libs7/oibl.scm
                  __main__/libs7/oibl/dune_actions.scm

                  EXCEPT: sometimes we don't get __main__. No idea
                  what controls.

                */
                const char *pfx;
                int pfx_len;
                if (dev_mode) {
                    pfx = "oibl/libs7/";
                    pfx_len = 11;
                    if (strncmp(dest, pfx, pfx_len) == 0) {
                        if (strlen(dirname(dest)) == pfx_len - 1) {
                            /* no path after pfx */
                            destdir = dirname(dest) + pfx_len;
                            xdg_install(token, ".");
                        } else {
                            destdir = dirname(dest) + pfx_len;
                            xdg_install(token, destdir);
                        }
                    } else {
                        /* log_debug("checking libs7"); */
                        pfx = "libs7/libs7/";
                        pfx_len = 12;
                        if (strncmp(dest, pfx, pfx_len) == 0) {
                            if (strlen(dirname(dest)) == pfx_len - 1) {
                                /* no path after pfx */
                                destdir = dirname(dest) + pfx_len;
                                xdg_install(token, ".");
                            } else {
                                destdir = dirname(dest) + pfx_len;
                                xdg_install(token, destdir);
                            }
                        }
                    }
                } else {
                    /* external mode */
                    pfx = "__main__/external/oibl/libs7/";
                    pfx_len = 32;
                    if (strncmp(dest, pfx, pfx_len) == 0) {
                        if (strlen(dirname(dest)) == pfx_len - 1) {
                            /* no path after pfx */
                            log_debug("OIBL to: .");
                            destdir = dirname(dest) + pfx_len;
                            xdg_install(token, ".");
                        } else {
                            log_debug("OIBL to: %s", destdir);
                            destdir = dirname(dest) + pfx_len;
                            xdg_install(token, destdir);
                        }
                    } else {
                        if (trace)
                            log_debug("checking libs7");
                        pfx = "__main__/external/libs7/libs7/";
                        pfx_len = 30;
                        if (strncmp(dest, pfx, pfx_len) == 0) {
                            if (strlen(dirname(dest)) == pfx_len - 1) {
                                /* no path after pfx */
                                destdir = dirname(dest) + pfx_len;
                                xdg_install(token, ".");
                            } else {
                                destdir = dirname(dest) + pfx_len;
                                xdg_install(token, destdir);
                            }
                        }
                    }
                }
#define LIBS7_PFX "libs7/libs7/"
const int libs7_pfx_len = 12;

#define OIBL_PFX "oibl/libs7/"
const int oibl_pfx_len = 11;

#define MAIN_PFX "__main__/"
const int main_pfx_len = 9;

                if (strncmp(dest, OIBL_PFX, oibl_pfx_len) == 0) {

                    if (strlen(destdir) == oibl_pfx_len - 1) {
                        /* log_debug("destdir: %s", destdir); */
                    } else {
                        /* log_debug("destdir: %s", destdir + oibl_pfx_len); */
                    }
                }

                else if (strncmp(dest,
                                 MAIN_PFX OIBL_PFX,
                                 main_pfx_len + oibl_pfx_len) == 0) {
                    if (strlen(destdir) == main_pfx_len + oibl_pfx_len - 1) {
                        /* pfx len includes trailing '/' */
                        /* destdir = "."; */
                        log_debug("destdir: %s", destdir);
                    } else {
                        log_debug("destdir: %s",
                                  destdir + main_pfx_len + oibl_pfx_len);
                    }
                }

                // libs7
                // __main__/external/libs7/libs7/alist.scm
                // __main__/external/libs7/libs7/s7/case.scm
                // pfx: __main__/external/libs7/libs7/
                // AND
                // libs7/libs7/alist.scm
                // libs7/libs7/s7/case.scm
                // pfx: libs7/libs7/

                else if (strncmp(dest, LIBS7_PFX, libs7_pfx_len) == 0) {
                    if (strlen(destdir) == libs7_pfx_len - 1) {
                        /* pfx len includes trailing '/' */
                        /* destdir = "."; */
                        /* log_debug("destdir: %s", destdir); */
                    } else {
                        /* log_debug("DESTDIR: %s", destdir + libs7_pfx_len); */
                    }
                }
                else if (strncmp(dest,
                                 MAIN_PFX LIBS7_PFX,
                                 main_pfx_len + libs7_pfx_len) == 0) {
                    if (strlen(destdir) == main_pfx_len + libs7_pfx_len - 1) {
                        /* pfx len includes trailing '/' */
                        destdir = ".";
                        /* log_debug("destdir: %s", destdir); */
                    } else {
                        /* log_debug("destdir: %s", */
                        /*           destdir + main_pfx_len + libs7_pfx_len); */
                    }
                }

                char *scriptdir = dirname(token);
                /* log_info("SCRIPTDIR: %s", scriptdir); */

                char *substr = strstr(scriptdir, "dune_ed/scm");
                if (substr != NULL) {
                    /* log_debug("FOUND dune_ed path: %s, %s", */
                    /*           line, scriptdir); */
                    if (dune_ed_scm == NULL) {
                        int len = strlen(scriptdir) + 1;
                        dune_ed_scm = calloc(len, 1);
                        strlcpy(dune_ed_scm, scriptdir, len);
                    }
                    continue;
                }

                /* sdir = s7_make_string(s7, scriptdir); */
                /* s7_pointer r = s7_hash_table_ref(s7, load_dirs, sdir); */
                /* if (r == s7_f(s7)) { */
                /*     // add to hash to ensure uniqueness */
                /*     /\* log_debug("adding to hash"); *\/ */
                /*     s7_hash_table_set(s7, load_dirs, */
                /*                       sdir, s7_t(s7)); */

                /*     tmp_load_path = s7_append(s7, tmp_load_path, */
                /*                     s7_list(s7, 1, */
                /*                             s7_make_string(s7, scriptdir))); */
                /* } */
            }
        }
    }
    fclose(fp);
    /* log_debug("cwd: %s", getcwd(NULL, 0)); */
}

LOCAL void xdg_install(char *src, char *dst)
{
    if (trace) {
        log_debug("xdg_install src: %s", src);
        log_debug("xdg_install_dir: %s", utstring_body(xdg_home_install_root));
    }
    rc = access(src, R_OK);
    if (rc) {
        if (verbose || debug)
            log_info("src not found: %s", src);
    }

    mkdir_r(utstring_body(xdg_home_install_root), dst);

    static UT_string *destdir = NULL;
    utstring_renew(destdir);
    utstring_concat(destdir, xdg_home_install_root);
    utstring_printf(destdir, "/%s", dst);
    if (trace)
        log_debug("install_dir: %s", utstring_body(destdir));
    _copy_file(src, utstring_body(destdir));
}

LOCAL void _config_xdg_home_load_paths()
{
    /*
      xdg home $XDG_DATA_HOME default: $HOME/.local/share

      xdg home bin:
      $XDG_DATA_HOME/.local/bin
      scripts:
      $XDG_DATA_HOME/libs7/
      $XDG_DATA_HOME/libs7/dune,
      $XDG_DATA_HOME/libs7/meta,
      $XDG_DATA_HOME/libs7/opam,
      $XDG_DATA_HOME/libs7/s7
    */
/* https://practical.li/blog/posts/adopt-FreeDesktop.org-XDG-standard-for-configuration-files/ */

/* https://developer.apple.com/library/archive/documentation/FileManagement/Conceptual/FileSystemProgrammingGuide/MacOSXDirectories/MacOSXDirectories.html#//apple_ref/doc/uid/TP40010672-CH10-SW1 */

    /* struct passwd *pw = getpwuid(getuid()); */
    /* const char *homedir = pw->pw_dir; */
    UT_string *xdg_data_home;
    utstring_new(xdg_data_home);
    char *tmp = getenv("XDG_DATA_HOME");
    if (tmp == NULL) {
        utstring_printf(xdg_data_home, "%s", getenv("HOME"));
    } else {
        utstring_printf(xdg_data_home, "%s", tmp);
    }
    if (trace)
        log_debug("xdg_data_home: %s", utstring_body(xdg_data_home));

    /* xdg_home_install_root is fixed */
    utstring_new(xdg_home_install_root);
    utstring_printf(xdg_home_install_root,
                    "%s/.local/share/libs7",
                    utstring_body(xdg_data_home));
    if (trace)
        log_debug("xdg_home_install_root: %s",
              utstring_body(xdg_home_install_root));

    /* xdg_install_dir varies, usually subdir of xdg_home_install_root */
    utstring_new(xdg_install_dir);

    /* xdg home bin */
    utstring_new(xdg_home_bin);
    utstring_printf(xdg_home_bin, "%s/.local/bin",
                    utstring_body(xdg_data_home));
    rc = access(utstring_body(xdg_home_bin), R_OK);
    if (rc) {
        if (verbose || debug)
            log_info("Creating: %s", utstring_body(xdg_home_bin));
        mkdir_r(utstring_body(xdg_data_home), ".local/bin");
    } else {
        if (verbose)
            log_debug("FOUND XDG: %s",
                      utstring_body(xdg_home_bin));
    }

    utstring_renew(xdg_install_dir);
    utstring_printf(xdg_install_dir, "%s/s7",
                    utstring_body(xdg_home_install_root));
    rc = access(utstring_body(xdg_install_dir), R_OK);
    if (rc) {
        if (verbose || debug)
            log_info("Creating: %s", utstring_body(xdg_install_dir));
        mkdir_r(utstring_body(xdg_home_install_root), "/s7");
    } else {
        if (verbose)
            log_debug("FOUND XDG: %s",
                     utstring_body(xdg_install_dir));
    }

    /* **** libs7/s7 **** */
    /* utstring_renew(xdg_install_dir); */
    /* utstring_printf(xdg_install_dir, "%s", */
    /*                 utstring_body(xdg_home_install_root)); */
    /* rc = access(utstring_body(xdg_install_dir), R_OK); */
    /* if (rc) { */
    /*     if (verbose || debug) */
    /*         log_info("Creating: %s", utstring_body(xdg_install_dir)); */
    /*     mkdir_r(utstring_body(xdg_home_install_root), ""); */
    /* } else { */
    /*     if (verbose) */
    /*         log_debug("FOUND XDG: %s", */
    /*                  utstring_body(xdg_install_dir)); */
    /* } */

    /* utstring_printf(xdg_install_dir, "%s/lib", */
    /*                 utstring_body(xdg_home_install_root)); */
    /* rc = access(utstring_body(xdg_install_dir), R_OK); */
    /* if (rc) { */
    /*     if (verbose || debug) */
    /*         log_info("Creating: %s", utstring_body(xdg_install_dir)); */
    /*     mkdir_r(utstring_body(xdg_home_install_root), ".local/share/libs7/lib"); */
    /* } else { */
    /*     if (verbose) */
    /*         log_debug("FOUND XDG: %s", */
    /*                  utstring_body(xdg_install_dir)); */
    /* } */

    /* **** oibl: dune **** */
    utstring_renew(xdg_install_dir);
    utstring_printf(xdg_install_dir, "%s/dune",
                    utstring_body(xdg_home_install_root));
    rc = access(utstring_body(xdg_install_dir), R_OK);
    if (rc) {
        if (verbose || debug)
            log_info("Creating: %s.", utstring_body(xdg_install_dir));
        mkdir_r(utstring_body(xdg_home_install_root), "/dune");
    } else {
        if (verbose)
            log_debug("FOUND XDG: %s",
                     utstring_body(xdg_install_dir));
    }

    /* **** oibl: meta **** */
    utstring_renew(xdg_install_dir);
    utstring_printf(xdg_install_dir, "%s/meta",
                    utstring_body(xdg_home_install_root));
    rc = access(utstring_body(xdg_install_dir), R_OK);
    if (rc) {
        if (verbose || debug)
            log_info("Creating: %s.", utstring_body(xdg_install_dir));
        mkdir_r(utstring_body(xdg_home_install_root), "/meta");
    } else {
        if (verbose)
            log_debug("FOUND XDG: %s",
                     utstring_body(xdg_install_dir));
    }
    /* **** oibl: opam **** */
    utstring_renew(xdg_install_dir);
    utstring_printf(xdg_install_dir, "%s/opam",
                    utstring_body(xdg_home_install_root));
    rc = access(utstring_body(xdg_install_dir), R_OK);
    if (rc) {
        if (verbose || debug)
            log_info("Creating: %s.", utstring_body(xdg_install_dir));
        mkdir_r(utstring_body(xdg_home_install_root), "/opam");
    } else {
        if (verbose)
            log_debug("FOUND XDG: %s",
                     utstring_body(xdg_install_dir));
    }
}

LOCAL void _config_xdg_sys_load_paths()
{
    /*
      xdg sys dirs:
       $XDG_DATA_DIRS/oibl, $XDG_DATA_DIRS/libs7, $XDG_DATA_DIRS/libs7/s7
       $XDG_DATA_DIRS default: /usr/local/share

       macos:
       user xdg $XDG_DATA_HOME default: $HOME/Library
       or: $HOME/Library/Application Support
    */
/* https://practical.li/blog/posts/adopt-FreeDesktop.org-XDG-standard-for-configuration-files/ */

/* https://developer.apple.com/library/archive/documentation/FileManagement/Conceptual/FileSystemProgrammingGuide/MacOSXDirectories/MacOSXDirectories.html#//apple_ref/doc/uid/TP40010672-CH10-SW1 */

    /* char *xdg_data_dirs = getenv("XDG_DATA_DIRS"); */
    /* if (xdg_data_dirs == NULL) { */
    /*     xdg_data_dirs = "/usr/local/share"; */
    /* } */

    /* utstring_new(xdg_install_dir); */
    /* utstring_printf(xdg_install_dir, */
    /*                 "%s/libs7/s7", */
    /*                 xdg_data_dirs); */
    /* rc = access(utstring_body(xdg_install_dir), R_OK); */
    /* if (rc) { */
    /*     if (verbose || debug) */
    /*         log_info("Not found: %s", */
    /*                  utstring_body(xdg_install_dir)); */
    /*     //FIXME: mkdir_r */
    /* } else { */
    /*     if (verbose) */
    /*         log_debug("FOUND XDG: %s", */
    /*                  utstring_body(xdg_install_dir)); */
    /* } */

    /* utstring_new(xdg_install_dir); */
    /* utstring_printf(xdg_install_dir, "%s/libs7", xdg_data_dirs); */
    /* rc = access(utstring_body(xdg_install_dir), R_OK); */
    /* if (rc) { */
    /*     if (verbose || debug) */
    /*         log_info("Not found: %s/libs7", */
    /*                  utstring_body(xdg_install_dir)); */
    /* } else { */
    /* } */

}

LOCAL void _copy_file(char *src, char *dst)
{
#define BUFSZ 4096

    unsigned char buffer[4096];

    FILE *inFp = fopen(src, "r");
    fseek(inFp, 0, SEEK_END);
    uint64_t fileSize = ftell(inFp);
    rewind(inFp);

    static UT_string *outpath;
    utstring_renew(outpath);
    if (strncmp(basename(src), "repl", 4) == 0) {
        char *bn = basename(src);
        if (strlen(bn) == 4) {
            utstring_printf(outpath, "%s/oibl", dst);
        } else {
            utstring_printf(outpath, "%s/%s", dst, basename(src));
        }
    } else {
        utstring_printf(outpath, "%s/%s", dst, basename(src));
    }
    /* log_debug("opening for write: %s", utstring_body(outpath)); */

    errno = 0;
    FILE *outFp = fopen(utstring_body(outpath), "w");
    if (outFp == NULL) {
        log_error("fopen fail for %s, err: %s",
                  utstring_body(outpath),
                  strerror(errno));
    }

    uint64_t outFileSizeCounter = fileSize;

    /* we fread() bytes from inFp in COPY_BUFFER_MAXSIZE increments,
       until there is nothing left to fread() */

    do {
        if (outFileSizeCounter > BUFSZ) {
            fread(buffer, 1, (size_t) BUFSZ, inFp);
            /* log_debug("writing"); */
            fwrite(buffer, 1, (size_t) BUFSZ, outFp);
            outFileSizeCounter -= BUFSZ;
        }
        else {
            fread(buffer, 1, (size_t) outFileSizeCounter, inFp);
            fwrite(buffer, 1, (size_t) outFileSizeCounter, outFp);
            outFileSizeCounter = 0ULL;
        }
    } while (outFileSizeCounter > 0);
    fclose(inFp);
    fclose(outFp);

    if (strncmp(basename(src), "repl", 4) == 0) {
        char *bn = basename(src);
        if (strlen(bn) == 4) {
            if (trace)
                log_debug("setting permissions on %s", utstring_body(outpath));
            chmod(utstring_body(outpath), S_IRWXU | S_IRGRP | S_IXGRP);
        }
    }
}
