module EmacsVtermTap
  TAP_OWNER = "binbinsh".freeze
  TAP_REPO = "emacs-vterm".freeze

  class UrlResolver
    def initialize(version, mode)
      mode = ENV["HOMEBREW_EMACS_VTERM_MODE"] || mode
      tap = Tap.fetch(TAP_OWNER, TAP_REPO)
      @version = version
      formula_name = "#{TAP_REPO}@#{version}"
      @formula_root =
        if mode == "local" || !tap.installed?
          Dir.pwd
        else
          tap.path.to_s.delete_suffix "/Formula/#{formula_name}.rb"
        end
    end

    def patch_url(name)
      "#{@formula_root}/patches/emacs-#@version/#{name}.patch"
    end

    def icon_url(name)
      "#{@formula_root}/icons/#{name}.icns"
    end

    def png_icon_url(name)
      "#{@formula_root}/icons/#{name}.png"
    end

    def tahoe_icon_url(name)
      "#{@formula_root}/icons/#{name}.icon"
    end

    def site_lisp_file(name)
      "#{@formula_root}/site-lisp/#{name}"
    end
  end
end
