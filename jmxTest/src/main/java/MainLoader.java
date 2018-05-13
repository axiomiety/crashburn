import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.management.MBeanServer;
import javax.management.ObjectName;
import java.lang.management.ManagementFactory;

public class MainLoader {
    private static final Logger LOGGER = LogManager.getLogger(MainLoader.class);
    private static final int SLEEP_INTERVAL = 5*1000; // 5 seconds

    public static void main(String[] args) throws Exception {
        MBeanServer server = ManagementFactory.getPlatformMBeanServer();
        ObjectName name = new ObjectName("jmxTest:type=MyController");
        MyController controller = new MyController();
        server.registerMBean(controller, name);

        while (controller.isRunning()) {
            LOGGER.info("*yawn* going back to sleep");
            Thread.sleep(SLEEP_INTERVAL);
        }
        LOGGER.info("shutting down...");
    }
}
