import React, { Component } from 'react';
import Window from '../../common/Window/Window';
import Picker from '../Picker';
import Editor from './containers/Editor'
import styles from './TfEditor.scss'
import { connect } from 'react-redux';
import ColorPicker from './ColorPicker';
import DataManager from '../../../api/DataManager';
import { TransferFunctionKey, HistogramKey } from '../../../api/keys';

class TfEditor extends Component {

  constructor(props) {
    super(props);

    this.state = {
      showTfEditor: false,
      showColorPicker: false,
      currentColor: '#ffffff',
    }
    this.toggleTfEditor = this.toggleTfEditor.bind(this);
    this.toggleColorPicker = this.toggleColorPicker.bind(this);
  }

  toggleTfEditor() {
    this.setState({ showTfEditor: !this.state.showTfEditor });
    this.setState({ showColorPicker: !this.state.showTfEditor });
  }

  toggleColorPicker() {
    this.setState({ showColorPicker: !this.state.showColorPicker });
  }

  render() {
    const {showTfEditor, showColorPicker, volumes} = this.state;
    return(
      <div className={styles.Wrapper}>
        <Picker onClick={this.toggleTfEditor} className={(showTfEditor ? styles.Active : '')}>
          <h2>TF</h2>
        </Picker>
         { showTfEditor && (
          <div>
          <Window size={{ width: 900, height: 700 }} closeCallback={this.toggleTfEditor} title={"Transfer Function Editor"} className={styles.Window}>
              <div className={styles.Canvas}>
                  <Editor tfId={this.props.transferfunctions} color={this.state.currentColor}/>
              </div>
          </Window>
          { showColorPicker && (
            <Window  size={{ width: 230, height: 350 }} closeCallback={this.toggleColorPicker} title="Color Picker" className={styles.ColorPickerWindow}>
              <ColorPicker onColorChange={(color) => this.setState({currentColor : color})} />
            </Window>
          )}
          </div>
        )}
      </div>
    );
  }
}

export default TfEditor;
