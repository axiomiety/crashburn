public interface MyControllerMBean {
    public void say(String s, String level);
    public String getLogLevel();
    public void setLogLevel(String level);
    public void exit();
}
