package jam.sweethome3d;

import com.eteks.sweethome3d.plugin.Plugin;
import com.eteks.sweethome3d.plugin.PluginAction;

public class NReplPlugin extends Plugin {
    @Override
    public PluginAction[] getActions() {
        return new PluginAction [] {new NReplAction(this)};
    }
}
