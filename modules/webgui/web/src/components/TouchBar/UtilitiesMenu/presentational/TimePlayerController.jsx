import React, { Component } from 'react';
import { CurrenTimeKey, DeltaTime, FastRewind, Rewind, Play, Forward, FastForward } from '../../../../api/keys';
import DataManager, { TopicTypes } from '../../../../api/DataManager';
import styles from './../style/TimeController.scss';
import buttonStyles from './../style/UtilitiesButtons.scss';
import * as timeHelpers from '../../../../utils/timeHelpers';
import Icon from '../../../common/Icon/Icon';
import SmallLabel from '../../../common/SmallLabel/SmallLabel';

const FastSpeed = 86400;
const Speed = 3600;

class TimePlayerController extends Component {
  constructor(props) {
    super(props);

    this.mounted = false;

    this.state = {
      paused: false,
      time: new Date(),
      subscriptionIdCurrent: -1,
      subscriptionIdDelta: -1,
      subscriptionIdPaused: -1,
      deltaTime: 1,
      activePlayer: '',
    };

    this.currentTimeCallback = this.currentTimeCallback.bind(this);
    this.deltaTimeCallback = this.deltaTimeCallback.bind(this);
    this.setSimulationSpeed = this.setSimulationSpeed.bind(this);

    this.setDate = this.setDate.bind(this);
    this.clickPlayer = this.clickPlayer.bind(this);
  }


  componentDidMount() {
    this.mounted = true;
    // subscribe to data
    this.state.subscriptionIdCurrent = DataManager
      .subscribe(CurrenTimeKey, this.currentTimeCallback, TopicTypes.time);
    this.state.subscriptionIdDelta = DataManager
      .subscribe(DeltaTime, this.deltaTimeCallback, TopicTypes.time);
  }

  componentWillUnmount() {
    // TODO this.mounted is a quick fix, because the unsubsciption of time does not work
    this.mounted = false;
    // Unsubscribe to data
    // DataManager.unsubscribe(CurrenTimeKey, this.state.subscriptionIdCurrent);
    // DataManager.unsubscribe(DeltaTime, this.state.subscriptionIdDelta);
    // this.setState({ subscriptionIdCurrent: -1, subscriptionIdDelta: -1 });
  }

  get time() {
    return this.state.time.toUTCString();
  }

  setDate(time) {
    if (this.mounted) {
      this.setState({ time });
      timeHelpers.setDate(time);
    }
  }

  setSimulationSpeed(speed) {
    if (this.mounted) {
      timeHelpers.UpdateDeltaTimeNow(speed);
      this.setState({ paused: false });
    }
  }

  deltaTimeCallback(message) {
    if (this.mounted) {
      this.setState({ deltaTime: message.deltaTime });
      // Translate delta time to a player button
      switch (message.deltaTime) {
        case 1: this.setState({ paused: false, activePlayer: Play }); break;
        case 0: this.setState({ paused: true, activePlayer: Play }); break;
        case Speed: this.setState({ activePlayer: Forward }); break;
        case -Speed: this.setState({ activePlayer: Rewind }); break;
        case FastSpeed: this.setState({ activePlayer: FastForward }); break;
        case -FastSpeed: this.setState({ activePlayer: FastRewind }); break;
        default: break;
      }
    }
  }

  currentTimeCallback(message) {
    if (this.mounted) {
      const time = new Date(timeHelpers.DateStringWithTimeZone(message.time));
      this.setState({ time });
    }
  }

  clickPlayer(e) {
    this.setState({ activePlayer: e.target.id });
    switch (e.target.id) {
      case FastRewind: this.setSimulationSpeed(-FastSpeed); break;
      case Rewind: this.setSimulationSpeed(-Speed); break;
      case Play:
        if (this.state.deltaTime !== 0) {
          this.setState({ paused: true });
          timeHelpers.UpdateDeltaTimeNow(0);
        } else this.setSimulationSpeed(1);
        break;
      case Forward: this.setSimulationSpeed(Speed); break;
      case FastForward: this.setSimulationSpeed(FastSpeed); break;
      default: break;
    }
  }

  render() {
    return (
      <div className={styles.TimeController}>
        <div className={styles.ButtonContainer}>
          <div
            className={buttonStyles.UtilitiesButton}
            onClick={timeHelpers.now}
            role="button"
            tabIndex="0"
          >
            <Icon icon="replay" className={styles.Icon} />
            <SmallLabel>Time</SmallLabel>
          </div>
        </div>
        <div className={styles.SimulationIncrement}>
          <div className={styles.TimeText}>
            {new Date(this.time).toLocaleString()}
          </div>
          <div className={styles.PlayerContainer}>
            <Icon icon="fast_rewind" id={FastRewind} className={`${styles.Icon} ${(this.state.activePlayer === FastRewind) && styles.active}`} onClick={this.clickPlayer} />
            <Icon icon="fast_rewind" id={Rewind} className={`${styles.Icon} ${(this.state.activePlayer === Rewind) && styles.active}`} onClick={this.clickPlayer} />
            <Icon icon={this.state.paused ? 'play_arrow' : 'pause'} id={Play} className={`${styles.Icon} ${(this.state.activePlayer === Play) && styles.active}`} onClick={this.clickPlayer} />
            <Icon icon="fast_forward" id={Forward} className={`${styles.Icon} ${(this.state.activePlayer === Forward) && styles.active}`} onClick={this.clickPlayer} />
            <Icon icon="fast_forward" id={FastForward} className={`${styles.Icon} ${(this.state.activePlayer === FastForward) && styles.active}`} onClick={this.clickPlayer} />
          </div>
        </div>
      </div>
    );
  }
}

export default TimePlayerController;
