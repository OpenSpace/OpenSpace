import React from 'react';
import Property from './Property';
import Button from '../../common/Input/Button/Button';
import DataManager from '../../../api/DataManager';

class TriggerProperty extends Property {
  onChange() {
    DataManager.trigger(this.uri);
  }

  render() {
    const { Name } = this.props.Description;
    return (
      <div>
        <Button onClick={this.onChange}>
          { Name }
        </Button>
        { this.descriptionPopup }
      </div>
    );
  }
}

export default TriggerProperty;
