import React, { Component } from 'react';
import { Link } from 'react-router-dom';
import { connect } from 'react-redux';
import Proptypes from 'prop-types'; 

import DataItemList from './presentational/DataItemList';
import { stringListToArray } from './utils/helpers';

import DataManager from '../../api/DataManager';
import styles from './DataLoader.scss';
import Button from '../common/Input/Button/Button';
import Label from '../common/Label/Label';
import UploadDataButton from './UploadDataButton';
import provideWindowWidth from './HOC/provideWindowSize';
import Popover from '../common/Popover/Popover';
import Picker from '../../components/BottomBar/Picker';

class DataLoader extends Component {
  constructor(props) {
    super(props);

    this.dataTypesToLoad = ['Volumes', 'Fieldlines'];

    this.handleDataTypeList = this.handleDataTypeList.bind(this);
    this.popover = this.popover.bind(this);

    this.state = {
      activeDataType: '',
      dataToLoadUri: '',
      dataItems: [],
      aButtonIsPressed: false,
      showPopover: false
    };
  }

  popover() {
    const { width, height } = this.props
    const { dataItems, aButtonIsPressed, showPopover } = this.state;
    const minSize = 700;

    const position = {
      x: width < minSize ? 0 : 100,
      y: height < minSize ? 0 : 300
    };

    let DataTypeButtons = () => {
      return(
        <section className={styles.dataButtons}>
          <div>
          {this.dataTypesToLoad.map(dataType => (
            <Button key={dataType} 
                    onClick={() => this.setState({activeDataType: dataType, aButtonIsPressed: true})}
                    disabled={dataType !== 'Volumes'}>
              <Label>{dataType}</Label>
            </Button>
          ))}
          </div>
        </section>
      );
    };

    return(
      <Popover
        className={Picker.Popover}
        title="DATA"
        closeCallback={() => this.togglePopover()}
        detachable={true}
      >
          <div className={styles.container}>
            <div className={styles.upload}>
              <Label size="large">Upload data from disk</Label>
              <div className={styles.buttons}>
                <UploadDataButton/>
              </div>
            </div>
            <div className={styles.horizontalLine}/>
            <div className={styles.load}>
              <Label size="large">Load saved data</Label>
              <div className={styles.buttons}>
                <DataTypeButtons/>
              </div>
              { aButtonIsPressed && (
                <DataItemList items={dataItems} />
              )}
            </div>
          </div>
      </Popover>
    );  
  }

  componentDidUpdate(prevProps, prevState) {
    const { activeDataType, dataToLoadUri } = this.state;

    if ((prevState.activeDataType !== activeDataType) && (activeDataType !== '')) {
      this.triggerDataToLoad(activeDataType);

      const uri = this.getUriForDataToLoad(activeDataType);
      this.setState({ dataToLoadUri: uri }, this.subscribeToActiveUri(uri));
    }

    if (prevState.dataToLoadUri !== dataToLoadUri) {
      this.subscribeToActiveUri(dataToLoadUri);
    }
  }

  getUriForDataToLoad(dataType) {
    let uri = 'Modules.DataLoader.';

    for (const type of this.dataTypesToLoad) {
      if (dataType == type) {
        uri += type;
      }
    }

    return uri;
  }

  triggerDataToLoad(dataType) {
    DataManager.trigger(`Modules.DataLoader.ShowInternal${dataType}Trigger`)
  }

  handleDataTypeList(data) {
    this.setState({dataItems: stringListToArray(data.Value)});
  }

  subscribeToActiveUri(uri = '') {
    DataManager.subscribe(uri || this.state.dataToLoadUri, this.handleDataTypeList);
  }

  togglePopover() {
    this.setState({ showPopover: !this.state.showPopover });
  }
  
  render() {
    const { showPopover } = this.state;
    return(
      <div className={Picker.Wrapper}>
        <Picker onClick={() => this.togglePopover()}>
          <div className={`${styles.container}`}>
            <Label size='medium'>DATA LOADER</Label>
          </div>
        </Picker>
        
        { showPopover && this.popover() }
      </div>
    );
  }
}

export default provideWindowWidth(DataLoader);