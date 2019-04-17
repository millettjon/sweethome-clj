package jam.sweethome3d;

import java.io.PrintWriter;
import com.eteks.sweethome3d.plugin.Plugin;
import com.eteks.sweethome3d.plugin.PluginAction;
import clojure.java.api.Clojure;
import clojure.lang.IFn;
import clojure.lang.RT;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class NReplPlugin extends Plugin {
    final static Logger logger = LoggerFactory.getLogger(NReplPlugin.class);

    public static Plugin plugin;

    protected void startRepl() {
        /** Clojure uses the context class loader so we have to set it to the current classloader.
            See:
            - https://github.com/Echtzeitsysteme/java-refactoring-ttc/issues/13
            - https://stackoverflow.com/questions/13423048/why-does-clojure-use-the-context-classloader-by-default
        */
        // Set it here before Clojure.RT is used at all.
        // Don't bother to unset it since it needs to be set on UI threads later where most code is invoked.
        Thread.currentThread().setContextClassLoader(getClass().getClassLoader());

        try {
            IFn require = Clojure.var("clojure.core", "require");
            require.invoke(Clojure.read("clojure.tools.nrepl.server"));
            IFn server = Clojure.var("clojure.tools.nrepl.server", "start-server");

            Integer port = 0;
            Object serverInstance = server.invoke(Clojure.read(":port"), port);
            port = (Integer)RT.get(serverInstance, Clojure.read(":port"));

            // Write port to current directory so that tooling (e.g. cider) can find it.
            try (PrintWriter ps = new PrintWriter(".nrepl-port")) {
                ps.print(port);
            } 

            logger.info("Started clojure nREPL on port {}", port);
        }
        catch (Throwable e) {
            logger.error("Error starting nrepl", e);
        }
    }

    @Override
    public PluginAction[] getActions() {
        plugin = this;
        startRepl();
        return new PluginAction [] {};
    }
}
