import React, { Component } from 'react';
import SmallLabel from '../common/SmallLabel/SmallLabel';
import Icon from '../common/Icon/Icon';
import Picker from './Picker';

import styles from './OriginPicker.scss';

class OriginPicker extends Component {
  constructor(props) {
    super(props);

    this.state = {
      origin: 'Mercury',
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
          <span className={Picker.Name}>{ this.origin }</span>
          <SmallLabel>Origin</SmallLabel>
        </div>
      </Picker>
    );
  }
}

export default OriginPicker;
