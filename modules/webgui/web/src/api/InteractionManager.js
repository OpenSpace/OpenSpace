import DataManager, { TopicTypes } from './DataManager';

/**
 * Interface to tell OpenSpace things about the user's interactions, clicks, keyboard, etc
 */
class InteractionManager {
  /**
   * Openspace may ignore the user's interaction and choose not send it to the gui
   */
  static blur() {
    const message = DataManager.wrapMessage({
      type: TopicTypes.interaction,
      payload: {
        event: 'blur',
      },
    });
    DataManager.send(message);
  }

  /**
   * OpenSpace should send the user's interactions to the GUI
   */
  static focus() {
    const message = DataManager.wrapMessage({
      type: TopicTypes.interaction,
      payload: {
        event: 'focus',
      },
    });
    DataManager.send(message);
  }
}

export default InteractionManager;
