import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.core.config.Configurator;

public class MyController implements MyControllerMBean {
    private final Logger logger = LogManager.getLogger(MyController.class);
    private boolean running = false;

    public MyController() {
        running = true;
    }

    public void say(String s, String level) {
        logger.log(Level.getLevel(level), s);
    }

    public boolean isRunning() {
        return running;
    }

    public String getLogLevel() {
        return logger.getLevel().toString();
    }

    public synchronized void setLogLevel(String level) {
        Level logLevel = Level.getLevel(level);
        Configurator.setLevel(LogManager.getLogger(MyController.class).getName(), logLevel);
        logger.log(logger.getLevel(), "LogLevel is now set to " + getLogLevel().toString());
    }

    public void exit() {
        running = false;
    }
}
