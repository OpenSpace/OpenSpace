import React, { Component } from 'react';
import { CurrenTimeKey, DeltaTime, FastRewind, Rewind, Play, Forward, FastForward } from '../../../api/keys';
import DataManager, { TopicTypes } from '../../../api/DataManager';
import styles from './TimeController.scss';
import Button from '../../common/Input/Button/Button';
import * as timeHelpers from '../../../utils/timeHelpers';
import Icon from '../../common/Icon/Icon';
import SmallLabel from '../../common/SmallLabel/SmallLabel';

const FastSpeed = 86400;
const Speed = 3600;

class TimePlayerController extends Component {
  constructor(props) {
    super(props);

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
    // subscribe to data
    this.state.subscriptionIdCurrent = DataManager
      .subscribe(CurrenTimeKey, this.currentTimeCallback, TopicTypes.time);
    this.state.subscriptionIdDelta = DataManager
      .subscribe(DeltaTime, this.deltaTimeCallback, TopicTypes.time);
  }

  componentWillUnmount() {
    // Unsubscribe to data
    DataManager.unsubscribe(CurrenTimeKey, this.state.subscriptionIdCurrent);
    DataManager.unsubscribe(DeltaTime, this.state.subscriptionIdDelta);
    this.setState({ subscriptionIdCurrent: -1, subscriptionIdDelta: -1 });
  }

  get time() {
    return this.state.time.toUTCString();
  }

  setDate(time) {
    this.setState({ time });
    timeHelpers.setDate(time);
  }

  setSimulationSpeed(speed) {
    timeHelpers.UpdateDeltaTimeNow(speed);
    this.setState({ paused: false });
  }

  deltaTimeCallback(message) {
    this.setState({ deltaTime: message.deltaTime });
    // Translate delta time to a player button
    let active = '';
    if (message.deltaTime === 1) {
      this.setState({ paused: false });
      active = Play;
    } else if (message.deltaTime === 0) {
      this.setState({ paused: true });
      active = Play;
    } else if (message.deltaTime === Speed) {
      active = Forward;
    } else if (message.deltaTime === FastSpeed) {
      active = FastForward;
    } else if (message.deltaTime === -Speed) {
      active = Rewind;
    } else if (message.deltaTime === -FastSpeed) {
      active = FastRewind;
    }

    this.setState({ activePlayer: active });
  }

  currentTimeCallback(message) {
    const time = new Date(timeHelpers.DateStringWithTimeZone(message.time));
    this.setState({ time });
  }

  clickPlayer(e) {
    this.setState({ activePlayer: e.target.id });
    switch (e.target.id) {
      case FastRewind:
        this.setSimulationSpeed(-FastSpeed);
        break;
      case Rewind:
        this.setSimulationSpeed(-Speed);
        break;
      case Play:
        if (this.state.deltaTime !== 0) {
          this.setState({ paused: true });
          timeHelpers.UpdateDeltaTimeNow(0);
        } else this.setSimulationSpeed(1);
        break;
      case Forward:
        this.setSimulationSpeed(Speed);
        break;
      case FastForward:
        this.setSimulationSpeed(FastSpeed);
        break;
      default:
        break;
    }
  }

  render() {
    return (
      <div className={styles.TimeController}>
        <div className={styles.ButtonContainer}>
          <Button block smalltext flexgrow fixedwidth onClick={timeHelpers.now}>
            <Icon icon="replay" className={styles.Icon} />
            <SmallLabel>Time</SmallLabel>
          </Button>
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
