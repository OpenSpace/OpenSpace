import React, { Component } from 'react';

import SmallLabel from '../common/SmallLabel/SmallLabel';
import Icon from '../common/Icon/Icon';
import LoadingString from '../common/LoadingString/LoadingString';
import Picker from './Picker';
import DataManager from '../../api/DataManager';

import styles from './OriginPicker.scss';

const ORIGIN_KEY = 'NavigationHandler.origin';

class OriginPicker extends Component {
  constructor(props) {
    super(props);

    this.state = {
      origin: 'Mercury',
      hasOrigin: false,
    };

    this.updateOrigin = this.updateOrigin.bind(this);
  }

  componentDidMount() {
    DataManager.subscribe(ORIGIN_KEY, this.updateOrigin);
  }

  componentWillUnmount() {
    DataManager.unsubscribe(ORIGIN_KEY, this.updateOrigin);
  }

  updateOrigin(data) {
    const { Value } = data;
    this.setState({ origin: Value, hasOrigin: Value !== '' });
  }

  get icon() {
    // return this.state.origin ? null : (<Icon icon="language" styling="extralarge" />);
    return (<Icon icon="language" styling={`extralarge ${styles.Icon}`} />);
  }

  get origin() {
    return this.state.origin;
  }

  render() {
    return (
      <Picker>
        { this.icon }
        <div className={Picker.Title}>
          <span className={Picker.Name}>
            <LoadingString loading={!this.state.hasOrigin}>
              { this.origin }
            </LoadingString>
          </span>
          <SmallLabel>Focus</SmallLabel>
        </div>
      </Picker>
    );
  }
}

export default OriginPicker;
