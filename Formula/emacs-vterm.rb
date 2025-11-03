require_relative "../Library/EmacsBase"

class EmacsVterm < EmacsBase
  init 30
  url "https://ftpmirror.gnu.org/emacs/emacs-30.2.tar.xz"
  mirror "https://ftp.gnu.org/gnu/emacs/emacs-30.2.tar.xz"
  sha256 "b3f36f18a6dd2715713370166257de2fae01f9d38cfe878ced9b1e6ded5befd9"

  desc "Emacs VTerm as default terminal for macOS"
  homepage "https://github.com/binbinsh/homebrew-emacs-vterm"
  license "GPL-3.0-or-later"

  head do
    if ENV["HOMEBREW_EMACS_PLUS_30_REVISION"]
      url "https://github.com/emacs-mirror/emacs.git", revision: ENV["HOMEBREW_EMACS_PLUS_30_REVISION"]
    else
      url "https://github.com/emacs-mirror/emacs.git", branch: "emacs-30"
    end
  end

  #
  # Options
  #

  # Opt-out
  option "without-cocoa", "Build a non-Cocoa version of Emacs"

  # Opt-in
  option "with-ctags", "Don't remove the ctags executable that Emacs provides"
  option "with-x11", "Experimental: build with x11 support"
  option "with-debug", "Build with debug symbols and debugger friendly optimizations"
  option "with-xwidgets", "Experimental: build with xwidgets support"
  option "with-no-frame-refocus", "Disables frame re-focus (ie. closing one frame does not refocus another one)"
  option "with-compress-install", "Build with compressed install optimization"

  #
  # Dependencies
  #

  depends_on "make" => :build
  depends_on "autoconf" => :build
  depends_on "gnu-sed" => :build
  depends_on "gnu-tar" => :build
  depends_on "grep" => :build
  depends_on "awk" => :build
  depends_on "coreutils" => :build
  depends_on "pkg-config" => :build
  depends_on "texinfo" => :build
  depends_on "xz" => :build
  depends_on "m4" => :build
  depends_on "sqlite" => :build
  depends_on "gnutls"
  depends_on "librsvg"
  depends_on "little-cms2"
  depends_on "tree-sitter"
  depends_on "webp"
  depends_on "imagemagick" => :optional
  depends_on "dbus" => :optional
  depends_on "mailutils" => :optional
  # `libgccjit` and `gcc` are required when Emacs compiles `*.elc` files asynchronously (JIT)
  depends_on "libgccjit"
  depends_on "gcc"

  # VTerm build/runtime deps for emacs-libvterm
  depends_on "cmake"
  depends_on "libvterm"
  depends_on "libtool" => :build

  depends_on "gmp" => :build
  depends_on "libjpeg" => :build
  depends_on "zlib" => :build

  if build.with? "x11"
    depends_on "libxaw"
    depends_on "freetype" => :recommended
    depends_on "fontconfig" => :recommended
  end

  #
  # Incompatible options
  #

  if build.with?("xwidgets") && !(build.with?("cocoa") && build.without?("x11"))
    odie "--with-xwidgets is not available when building --with-x11"
  end

  #
  # URL (HEAD override)
  #

  if ENV["HOMEBREW_EMACS_PLUS_30_REVISION"]
    url "https://github.com/emacs-mirror/emacs.git", revision: ENV["HOMEBREW_EMACS_PLUS_30_REVISION"]
  end

  #
  # Icons
  #
  inject_icon_options

  # Default PNG icon resource (used if no --with-*-icon is selected)
  resource "emacs-vterm-icon-png" do
    url (@@urlResolver.png_icon_url "emacs-vterm"), using: CopyDownloadStrategy
    sha256 "2364d3468ecfde56ad412e93412e2621d4a367e5e5772c739d1152578f7fd52e"
  end

  #
  # Patches
  #

  opoo "The option --with-no-frame-refocus is not required anymore in emacs-vterm." if build.with? "no-frame-refocus"
  local_patch "fix-window-role", sha: "1f8423ea7e6e66c9ac6dd8e37b119972daa1264de00172a24a79a710efcb8130"
  local_patch "system-appearance", sha: "9eb3ce80640025bff96ebaeb5893430116368d6349f4eb0cb4ef8b3d58477db6"
  local_patch "round-undecorated-frame", sha: "7451f80f559840e54e6a052e55d1100778abc55f98f1d0c038a24e25773f2874"

  #
  # Install
  #

  def install
    args = %W[
      --disable-dependency-tracking
      --disable-silent-rules
      --enable-locallisppath=#{opt_prefix}/share/emacs/site-lisp
      --infodir=#{libexec}/info/emacs
      --prefix=#{libexec}
      --with-native-compilation=aot
    ]

    args << "--with-xml2"
    args << "--with-gnutls"

    args << "--without-compress-install" if build.without? "compress-install"

    ENV.append "CFLAGS", "-g -Og" if build.with? "debug"
    ENV.append "CFLAGS", "-O2 -DFD_SETSIZE=10000 -DDARWIN_UNLIMITED_SELECT"

    ENV.append "CFLAGS", "-I#{Formula["sqlite"].include}"
    ENV.append "LDFLAGS", "-L#{Formula["sqlite"].opt_lib}"

    # Necessary for libgccjit library discovery
    gcc_ver = Formula["gcc"].any_installed_version
    gcc_ver_major = gcc_ver.major
    gcc_lib="#{HOMEBREW_PREFIX}/lib/gcc/#{gcc_ver_major}"

    ENV.append "CFLAGS", "-I#{Formula["gcc"].include}"
    ENV.append "CFLAGS", "-I#{Formula["libgccjit"].include}"

    ENV.append "LDFLAGS", "-L#{gcc_lib}"
    ENV.append "LDFLAGS", "-Wl,-rpath,#{gcc_lib}"

    args <<
      if build.with? "dbus"
        "--with-dbus"
      else
        "--without-dbus"
      end

    # Note that if ./configure is passed --with-imagemagick but can't find the
    # library it does not fail but imagemagick support will not be available.
    # See: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=24455
    args <<
      if build.with?("imagemagick")
        "--with-imagemagick"
      else
        "--without-imagemagick"
      end

    if build.with? "imagemagick"
      imagemagick_lib_path = Formula["imagemagick"].opt_lib/"pkgconfig"
      ohai "ImageMagick PKG_CONFIG_PATH: ", imagemagick_lib_path
      ENV.prepend_path "PKG_CONFIG_PATH", imagemagick_lib_path
    end

    args << "--with-modules"
    args << "--with-rsvg"
    args << "--with-webp"
    args << "--without-pop" if build.with? "mailutils"
    args << "--with-xwidgets" if build.with? "xwidgets"

    system "./autogen.sh"

    if (build.with? "cocoa") && (build.without? "x11")
      args << "--with-ns" << "--disable-ns-self-contained"

      system "./configure", *args

      # Disable aligned_alloc on Mojave. See issue: https://github.com/daviderestivo/homebrew-emacs-head/issues/15
      if OS.mac? && MacOS.version <= :mojave
        ohai "Force disabling of aligned_alloc on macOS <= Mojave"
        configure_h_filtered = File.read("src/config.h")
                                   .gsub("#define HAVE_ALIGNED_ALLOC 1", "#undef HAVE_ALIGNED_ALLOC")
                                   .gsub("#define HAVE_DECL_ALIGNED_ALLOC 1", "#undef HAVE_DECL_ALIGNED_ALLOC")
                                   .gsub("#define HAVE_ALLOCA 1", "#undef HAVE_ALLOCA")
                                   .gsub("#define HAVE_ALLOCA_H 1", "#undef HAVE_ALLOCA_H")
        File.write("src/config.h", configure_h_filtered)
      end

      system "gmake"

      system "gmake", "install"

      icons_dir = buildpath/"nextstep/Emacs.app/Contents/Resources"

      selected_icon = false
      ICONS_CONFIG.each_key do |icon|
        next if build.without? "#{icon}-icon"

        selected_icon = true
        rm "#{icons_dir}/Emacs.icns"
        resource("#{icon}-icon").stage do
          icons_dir.install Dir["*.icns*"].first => "Emacs.icns"
        end
      end

      unless selected_icon
        resource("emacs-vterm-icon-png").stage do
          system "mkdir", "icon.iconset"
          sizes = [[16, 1], [16, 2], [32, 1], [32, 2], [128, 1], [128, 2], [256, 1], [256, 2], [512, 1], [512, 2]]
          sizes.each do |size, scale|
            dim = size * scale
            out = (scale == 1) ? "icon_#{size}x#{size}.png" : "icon_#{size}x#{size}@2x.png"
            system "sips", "-z", dim.to_s, dim.to_s, "emacs-vterm.png", "--out", File.join("icon.iconset", out)
          end
          system "iconutil", "-c", "icns", "icon.iconset", "-o", "Emacs.icns"
          icons_dir.install "Emacs.icns"
        end
      end

      prefix.install "nextstep/Emacs.app"
      (prefix/"Emacs.app/Contents").install "native-lisp"

      # inject PATH to Info.plist (while bundle name is Emacs.app)
      inject_path

      # inject description for protected resources usage (while Emacs.app)
      inject_protected_resources_usage_desc

      # Rename the bundle and update its metadata
      old_app = "#{prefix}/Emacs.app"
      new_app = "#{prefix}/Emacs VTerm.app"
      File.rename(old_app, new_app) unless File.exist?(new_app)

      plist = "#{new_app}/Contents/Info.plist"
      system "/usr/libexec/PlistBuddy -c 'Add CFBundleName string' '#{plist}' || true"
      system "/usr/libexec/PlistBuddy -c 'Set CFBundleName Emacs VTerm' '#{plist}'"
      system "/usr/libexec/PlistBuddy -c 'Add CFBundleDisplayName string' '#{plist}' || true"
      system "/usr/libexec/PlistBuddy -c 'Set CFBundleDisplayName Emacs VTerm' '#{plist}'"
      system "/usr/libexec/PlistBuddy -c 'Add CFBundleIdentifier string' '#{plist}' || true"
      system "/usr/libexec/PlistBuddy -c 'Set CFBundleIdentifier org.gnu.EmacsVTerm' '#{plist}'"
      # Compile and install layered Tahoe icon for macOS 26+
      install_tahoe_icon(new_app)
      system "touch '#{new_app}'"

      # Provide CLI shim (emacs-vterm only; do not install or override `emacs`)
      (bin/"emacs").unlink if (bin/"emacs").exist?
      (bin/"emacs-vterm").write <<~EOS
        #!/bin/bash
        exec "#{prefix}/Emacs VTerm.app/Contents/MacOS/Emacs" "$@"
      EOS

      # Install site-lisp into the app bundle Resources to avoid global share conflicts
      site_lisp_dir = Pathname.new(new_app).join("Contents/Resources/site-lisp")
      site_lisp_dir.mkpath
      File.write(site_lisp_dir/"emacs-vterm-start.el", <<~ELISP)
        ;; Auto-start vterm for GUI, no-args launches, while respecting user config
        (when (and (display-graphic-p)
                   (not noninteractive)
                   (not (daemonp))
                   (null command-line-args-left)
                   (not (bound-and-true-p initial-buffer-choice)))
          (setq initial-buffer-choice
                (lambda ()
                  (require 'vterm nil t)
                  (if (fboundp 'vterm)
                      (let* ((ts (format-time-string "%Y%m%d-%H%M%S"))
                             (buf (generate-new-buffer-name (format "*vterm-%s*" ts))))
                        (vterm buf))
                    (progn
                      (message "emacs-vterm: vterm not found. Install with M-x package-install RET vterm RET")
                      (get-buffer-create "*scratch*"))))))

        ;; Also open vterm on newly created GUI frames
        (add-hook 'after-make-frame-functions
                  (lambda (frame)
                    (when (and (display-graphic-p frame)
                               (not noninteractive)
                               (not (daemonp)))
                      (with-selected-frame frame
                        (require 'vterm nil t)
                        (when (fboundp 'vterm)
                          (let* ((ts (format-time-string "%Y%m%d-%H%M%S"))
                                 (buf (generate-new-buffer-name (format "*vterm-%s*" ts))))
                            (vterm buf)))))))
      ELISP

      site_start = site_lisp_dir/"site-start.el"
      loader = '(load "emacs-vterm-start" t t)'
      if site_start.exist?
        content = File.read(site_start)
        File.open(site_start, "a") { |ff| ff.puts loader } unless content.include?(loader)
      else
        File.write(site_start, loader + "\n")
      end
    else
      if build.with? "x11"
        # These libs are not specified in xft's .pc. See:
        # https://trac.macports.org/browser/trunk/dports/editors/emacs/Portfile#L74
        # https://github.com/Homebrew/homebrew/issues/8156
        ENV.append "LDFLAGS", "-lfreetype -lfontconfig"
        args << "--with-x"
        args << "--with-gif=no" << "--with-tiff=no" << "--with-jpeg=no"
      else
        args << "--without-x"
      end
      args << "--without-ns"

      system "./configure", *args

      # Disable aligned_alloc on Mojave. See issue: https://github.com/daviderestivo/homebrew-emacs-head/issues/15
      if OS.mac? && MacOS.version <= :mojave
        ohai "Force disabling of aligned_alloc on macOS <= Mojave"
        configure_h_filtered = File.read("src/config.h")
                                   .gsub("#define HAVE_ALIGNED_ALLOC 1", "#undef HAVE_ALIGNED_ALLOC")
                                   .gsub("#define HAVE_DECL_ALIGNED_ALLOC 1", "#undef HAVE_DECL_ALIGNED_ALLOC")
                                   .gsub("#define HAVE_ALLOCA 1", "#undef HAVE_ALLOCA")
                                   .gsub("#define HAVE_ALLOCA_H 1", "#undef HAVE_ALLOCA_H")
        File.write("src/config.h", configure_h_filtered)
      end

      system "gmake"
      system "gmake", "install"
    end
  end

  def post_install
    emacs_info_dir = info/"emacs"
    Dir.glob(emacs_info_dir/"*.info") do |info_filename|
      system "install-info", "--info-dir=#{emacs_info_dir}", info_filename
    end
  end

  def caveats
    <<~EOS
      Emacs VTerm.app was installed to:
        #{prefix}

      Your PATH value was injected into the app via a wrapper script.
      This solves the issue with macOS Sequoia ignoring LSEnvironment in Info.plist.

      To disable PATH injection, set EMACS_PLUS_NO_PATH_INJECTION before running:
        export EMACS_PLUS_NO_PATH_INJECTION=1

      Report any issues to https://github.com/binbinsh/homebrew-emacs-vterm
    EOS
  end

  service do
    run [opt_bin/"emacs-vterm", "--fg-daemon"]
    keep_alive true
    log_path "/tmp/homebrew.mxcl.emacs-vterm.stdout.log"
    error_log_path "/tmp/homebrew.mxcl.emacs-vterm.stderr.log"
  end

  test do
    assert_equal "4", shell_output("#{bin}/emacs-vterm --batch --eval=\"(print (+ 2 2))\"").strip
  end
end
