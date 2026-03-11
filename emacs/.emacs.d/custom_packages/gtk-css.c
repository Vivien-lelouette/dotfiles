/* gtk-css.c — Emacs dynamic module to inject GTK3 CSS at runtime.
   Provides (gtk-css-load CSS-STRING) to Emacs Lisp.  */

#include <emacs-module.h>
#include <gtk/gtk.h>
#include <stdio.h>
#include <string.h>

int plugin_is_GPL_compatible;

static GtkCssProvider *provider = NULL;
static int provider_registered = 0;

static emacs_value
Fgtk_css_load (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  (void)nargs; (void)data;

  ptrdiff_t len = 0;
  env->copy_string_contents (env, args[0], NULL, &len);
  char *css = malloc (len);
  if (!css)
    return env->intern (env, "nil");
  env->copy_string_contents (env, args[0], css, &len);

  if (!provider)
    provider = gtk_css_provider_new ();

  if (!provider_registered)
    {
      /* Try all available GdkDisplays and their screens.  */
      GdkDisplay *display = gdk_display_get_default ();
      if (display)
        {
          GdkScreen *screen = gdk_display_get_default_screen (display);
          if (screen)
            {
              gtk_style_context_add_provider_for_screen (
                screen, GTK_STYLE_PROVIDER (provider),
                GTK_STYLE_PROVIDER_PRIORITY_USER + 100);
              provider_registered = 1;
              fprintf (stderr, "gtk-css: registered provider on screen %p\n",
                       (void *)screen);
            }
          else
            fprintf (stderr, "gtk-css: no default screen\n");
        }
      else
        fprintf (stderr, "gtk-css: no default display\n");
    }

  GError *error = NULL;
  gtk_css_provider_load_from_data (provider, css, -1, &error);
  if (error)
    {
      fprintf (stderr, "gtk-css: CSS error: %s\n", error->message);
      g_error_free (error);
      free (css);
      return env->intern (env, "nil");
    }

  fprintf (stderr, "gtk-css: loaded %td bytes of CSS\n", len - 1);
  free (css);
  return env->intern (env, "t");
}

int
emacs_module_init (struct emacs_runtime *runtime)
{
  emacs_env *env = runtime->get_environment (runtime);

  emacs_value func = env->make_function (
    env, 1, 1, Fgtk_css_load,
    "Load CSS-STRING into the GTK3 default screen CSS provider.\n"
    "The CSS is applied immediately to all GTK widgets (scrollbars, etc.).",
    NULL);

  emacs_value symbol = env->intern (env, "gtk-css-load");
  emacs_value args[] = { symbol, func };
  env->funcall (env, env->intern (env, "defalias"), 2, args);

  emacs_value feature = env->intern (env, "gtk-css");
  emacs_value provide = env->intern (env, "provide");
  env->funcall (env, provide, 1, &feature);

  return 0;
}
