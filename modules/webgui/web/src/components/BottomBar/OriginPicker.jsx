import React, { Component } from 'react';
import SmallLabel from '../common/SmallLabel/SmallLabel';
import Icon from '../common/Icon/Icon';
import LoadingString from '../common/LoadingString/LoadingString';
import Picker from './Picker';

import styles from './OriginPicker.scss';

class OriginPicker extends Component {
  constructor(props) {
    super(props);

    this.state = {
      origin: 'Mercury',
      hasOrigin: false,
    };
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
          <SmallLabel>Origin</SmallLabel>
        </div>
      </Picker>
    );
  }
}

export default OriginPicker;
