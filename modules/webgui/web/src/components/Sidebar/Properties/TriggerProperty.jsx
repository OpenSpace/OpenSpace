import React from 'react';
import Property from './Property';
import Button from '../../common/Input/Button/Button';
import DataManager from '../../../api/DataManager';
import { connectProperty } from './connectProperty';

class TriggerProperty extends Property {
  constructor(props) {
    super(props);
    this.onChange = this.onChange.bind(this);
  }

  onChange() {
    console.log(this.props.Description.Identifier)
    this.props.ChangeValue();
  }

  render() {
    const { Name } = this.props.Description;
    console.log(this)
    return (
      <div>
        <Button onClick={this.props.onChange}>
          { Name }
        </Button> { this.descriptionPopup }
      </div>
    );
  }
}

TriggerProperty = connectProperty(TriggerProperty)

export default TriggerProperty;
