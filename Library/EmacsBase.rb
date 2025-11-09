require_relative "UrlResolver"
require_relative "Icons"
require "tmpdir"
require "fileutils"


class CopyDownloadStrategy < AbstractFileDownloadStrategy
  def initialize(url, name, version, **meta)
    @cached_location = Pathname.new url
  end
end

class EmacsBase < Formula
  def self.init version
    @@urlResolver = EmacsVtermTap::UrlResolver.new(version, ENV["HOMEBREW_EMACS_VTERM_MODE"] || "remote")
  end

  def self.local_patch(name, sha:)
    patch do
      url (@@urlResolver.patch_url name), :using => CopyDownloadStrategy
      sha256 sha
    end
  end

  def self.inject_icon_options
    EmacsVtermTap::ICONS_CONFIG.each do |icon, sha|
      option "with-#{icon}-icon", "Using Emacs #{icon} icon"
      next if build.without? "#{icon}-icon"
      resource "#{icon}-icon" do
        url (@@urlResolver.icon_url icon), :using => CopyDownloadStrategy
        sha256 sha
      end
    end
  end

  def inject_path
    ohai "Injecting PATH via wrapper script in Emacs.app/Contents/MacOS/Emacs"
    app = "#{prefix}/Emacs.app"
    emacs_binary = "#{app}/Contents/MacOS/Emacs"
    emacs_real = "#{app}/Contents/MacOS/Emacs-real"
    path = PATH.new(ORIGINAL_PATHS)

    puts "Creating wrapper script with following PATH value:"
    path.each_entry { |x|
      puts x
    }

    # Escape single quotes for use within single-quoted shell string
    # Replace ' with '\'' (end quote, escaped quote, start quote)
    escaped_path = path.to_s.gsub("'", "'\\''")

    # Rename original binary
    File.rename(emacs_binary, emacs_real) unless File.exist?(emacs_real)

    # Create wrapper script with relative path for relocatability
    File.open(emacs_binary, "w") do |f|
      f.write <<~EOS
        #!/bin/sh
        if [ -z "$EMACS_PLUS_NO_PATH_INJECTION" ]; then
          export PATH='#{escaped_path}'
        fi
        THIS_DIR="$(dirname "$0")"
        SITE_LISP="${THIS_DIR}/../Resources/site-lisp"
        if [ -d "$SITE_LISP" ]; then
          if [ -n "$EMACSLOADPATH" ]; then
            export EMACSLOADPATH="$SITE_LISP:$EMACSLOADPATH"
          else
            export EMACSLOADPATH="$SITE_LISP:"
          fi
        fi
        exec "$(dirname "$0")/Emacs-real" "$@"
      EOS
    end

    # Make executable
    File.chmod(0755, emacs_binary)
    system "touch '#{app}'"
  end

  def print_env
    ohai "Environment"
    ["CC",
     "CXX",
     "OBJC",
     "OBJCXX",
     "CFLAGS",
     "CXXFLAGS",
     "CPPFLAGS",
     "LDFLAGS",
     "SDKROOT",
     "MAKEFLAGS",
     "CMAKE_PREFIX_PATH",
     "CMAKE_FRAMEWORK_PATH",
     "PKG_CONFIG_PATH",
     "PKG_CONFIG_LIBDIR",
     "HOMEBREW_GIT",
     "ACLOCAL_PATH",
     "PATH",
     "CPATH",
    ].each { |key|
      puts "#{key}: #{ENV[key]}"
    }
  end

  def inject_protected_resources_usage_desc
    ohai "Injecting description for protected resources usage"
    app = "#{prefix}/Emacs.app"
    plist = "#{app}/Contents/Info.plist"

    system "/usr/libexec/PlistBuddy -c 'Add NSCameraUsageDescription string' '#{plist}'"
    system "/usr/libexec/PlistBuddy -c 'Set NSCameraUsageDescription Emacs requires permission to access the Camera.' '#{plist}'"
    system "/usr/libexec/PlistBuddy -c 'Add NSMicrophoneUsageDescription string' '#{plist}'"
    system "/usr/libexec/PlistBuddy -c 'Set NSMicrophoneUsageDescription Emacs requires permission to access the Microphone.' '#{plist}'"
    system "/usr/libexec/PlistBuddy -c 'Add NSSpeechRecognitionUsageDescription string' '#{plist}' || true"
    system "/usr/libexec/PlistBuddy -c 'Set NSSpeechRecognitionUsageDescription Emacs requires permission to handle any speech recognition.' '#{plist}' || true"
    system "touch '#{app}'"
  end

  def install_tahoe_icon(app_bundle_path)
    ohai "Compiling Tahoe layered icon for macOS 26 (if available)"

    # Locate App.icon in the tap
    tahoe_icon_dir = @@urlResolver.tahoe_icon_url "App"
    unless File.directory?(tahoe_icon_dir)
      opoo "No Tahoe icon directory found at #{tahoe_icon_dir}; skipping"
      return
    end

    # Ensure actool is available via Xcode Command Line Tools
    actool_path = `xcrun --find actool`.strip
    unless $?.success? && !actool_path.empty?
      opoo "xcrun actool not found; install Xcode Command Line Tools to build Tahoe icons"
      return
    end

    Dir.mktmpdir("tahoe_icon") do |tmpdir|
      system "xcrun", "actool", tahoe_icon_dir,
             "--compile", tmpdir,
             "--app-icon", "App",
             "--enable-on-demand-resources", "NO",
             "--development-region", "en",
             "--target-device", "mac",
             "--platform", "macosx",
             "--enable-icon-stack-fallback-generation=disabled",
             "--include-all-app-icons",
             "--minimum-deployment-target", "10.14",
             "--output-partial-info-plist", "/dev/null"

      assets_car = File.join(tmpdir, "Assets.car")
      if File.exist?(assets_car)
        resources_dir = Pathname.new(app_bundle_path).join("Contents/Resources")
        resources_dir.install assets_car

        plist = File.join(app_bundle_path, "Contents/Info.plist")
        system "/usr/libexec/PlistBuddy -c 'Add CFBundleIconName string' '#{plist}' || true"
        system "/usr/libexec/PlistBuddy -c 'Set CFBundleIconName App' '#{plist}'"
        system "touch '#{app_bundle_path}'"
      else
        opoo "actool did not produce Assets.car; skipping"
      end
    end
  end

  
end
