import React, { Component } from 'react';
import Window from '../../common/Window/Window';
import Picker from '../Picker';
import EditorCanvas from './EditorCanvas'
import styles from './TfEditor.scss'
import ColorPic from './ColorPicker';
import DataManager from '../../../api/DataManager';
import { TransferFunctionKey } from '../../../api/keys';

class TfEditor extends Component {

  constructor(props) {
    super(props);

    this.state = {
      properties: [],
      showTfEditor: false,
      showColorPicker: false,
      currentColor: 'blue',
    }
    this.toggleTfEditor = this.toggleTfEditor.bind(this);
    this.toggleColorPicker = this.toggleColorPicker.bind(this);
    this.receiveData = this.receiveData.bind(this);
  }

  toggleTfEditor() {
    this.setState({ showTfEditor: !this.state.showTfEditor });
    this.setState({ showColorPicker: !this.state.showTfEditor });
  }

  toggleColorPicker() {
    this.setState({ showColorPicker: !this.state.showColorPicker });
  }

  componentDidMount() {
    DataManager.getValue(TransferFunctionKey, this.receiveData('properties'));
  }

  receiveData(prop) {
    return ({ value }) => this.setState({ [prop]: value, hasData: true });
  }

  render() {
    const showTfEditor = this.state.showTfEditor;
    const showColorPicker = this.state.showColorPicker;
    return(
      <div className={styles.Wrapper}>
        <Picker onClick={this.toggleTfEditor} className={(showTfEditor ? styles.Active : '')}>
          <h2>TF</h2>
        </Picker>
         { showTfEditor && (
          <div>
          <Window size={{ width: 900, height: 700 }} closeCallback={this.toggleTfEditor} title="Transfer Function Editor" className={styles.Window}>
              <div className={styles.Canvas}>
                <EditorCanvas />
              </div>
          </Window>
          { showColorPicker && (
            <Window  size={{ width: 230, height: 350 }} closeCallback={this.toggleColorPicker} title="Color Picker" className={styles.ColorPickerWindow}>
              <ColorPic onColorChange={(color) => this.setState({currentColor : color})} />
            </Window>
          )}
          </div>
        )}
      </div>
    );
  }
}

export default TfEditor;
