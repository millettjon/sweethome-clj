package jam.sweethome3d;

import com.eteks.sweethome3d.plugin.Plugin;
import com.eteks.sweethome3d.plugin.PluginAction;
import javax.swing.JOptionPane;
import com.github.austinc.jnrepl.Jnrepl;

public class NReplAction extends PluginAction {
    public static Plugin plugin;

    public NReplAction(Plugin plugin_) {
        plugin = plugin_;
        putPropertyValue(Property.NAME, "Start NRepl server");
        putPropertyValue(Property.MENU, "Tools");
        // Enables the action by default
        setEnabled(true);

        // Clojure uses the context class loader so we have to set it to the current classloader.
        // See: https://github.com/Echtzeitsysteme/java-refactoring-ttc/issues/13
        Thread.currentThread().setContextClassLoader(plugin.getPluginClassLoader());
    }

    @Override
    public void execute() {
        Jnrepl.startRepl(9000);
        JOptionPane.showMessageDialog(null, "Started NRepl server on port 9000");
        // Jnrepl.shutdownRepl(); // shut it down
    }
}
