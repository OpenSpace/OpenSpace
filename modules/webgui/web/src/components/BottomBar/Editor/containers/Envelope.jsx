import React, {Component} from 'react'
import PropTypes from 'prop-types';
import Draggable from 'react-draggable'
import { connect } from 'react-redux';
import EnvelopeCanvas from '../presentational/EnvelopeCanvas'
import styles from '../style/Envelope.scss'
import GraphBody from '../../../common/Graph/GraphBody'
import Point from '../presentational/Point'
import PointPositionGraph from './PointPositionGraph'
import { toggleActiveEnvelope, toggleActivePoint, movePoint, swapPoints } from '../actions';

class Envelope extends Component {
  constructor(props) {
    super(props);
    this.handleDrag = this.handleDrag.bind(this);
    this.handleClick = this.handleClick.bind(this);
    this.checkForSwap = this.checkForSwap.bind(this);
  }

  checkForSwap(position, points, index) {
    if (points[index].anchor)
      return -1;

    if (position.x < points[index - 1].position.x){
      return points[index - 1].anchor ? -1 : points[index].id;
    }
    else if (position.x > points[index + 1].position.x){
      return points[index + 1].anchor ? -1 : points[index].id;
    }
    else
      return -1;
  }

  handleDrag(e, ui, index, envelope) {
    var id = envelope.points[index].id;
    if(ui.deltaX !== 0 && ui.deltaY !== 0) {
      envelope.points[index].clickable = false;
    }

    let position = {
      x: envelope.points[index].position.x + ui.deltaX,
      y: envelope.points[index].position.y,
    }

    if(!envelope.points[index].anchor)
      position.y = position.y + ui.deltaY;

    this.props.MovePoint(position, id, envelope.id);
    var swapMate = this.checkForSwap(position, envelope.points, index);

    if(swapMate !== -1) {
      //console.log("swap " + id + "  " + swapMate );
      this.props.SwapPoints(id, swapMate, envelope.id);
    }
  }

  handleClick(envelope, pointId) {
    if (envelope.points[pointId].clickable === false) {
      envelope.points[pointId].clickable = true;
    }
    else {
      if (envelope.active === true) {
        this.props.TogglePoint(envelope.id, pointId);
      }
      else if(this.hasActiveChild(envelope)) {
        this.props.TogglePoint(envelope.id, pointId);
      }
      else {
        this.props.ToggleEnvelope(envelope.id);
      }
    }
  }

  hasActiveChild(envelope) {
    var hasActiveChild = false;
    envelope.points.forEach(function(point) {
      if (point.active)
        hasActiveChild = true;
    })
    return hasActiveChild;
  }

  pointsToCanvas(points){
    return points.map((point, index) => ({
          ...point,
          position: {x: point.position.x - 10,
                    y: point.position.y - 10},
        })
    )
  }

  pointsForEnvelopeGraph(data){
    let convertedData = [];
    data.forEach(function(point) {
      let tmpObject = Object.assign({},
        {x: point.position.x ,
         y: 600 - point.position.y,
         color: point.color}
        )
      convertedData.push(tmpObject);
    })
    return convertedData;
  }

  render() {
    const { height, width, active, envelopes, minValue, maxValue} = this.props;
    return (
      <div className={styles.Envelope}>
      {(envelopes != undefined) && (
        <div>
        <PointPositionGraph className={styles.Envelope}
          {...this.props}
          envelopes={envelopes}
          minValue={minValue}
          maxValue={maxValue}
        />
        {envelopes.map(envelope =>
          <div key={envelope.id}>
          <svg className={styles.Line} height={height} width={width + 10}>
            <GraphBody
             UseLinearGradient={true}
             points={this.pointsForEnvelopeGraph(envelope.points)}
             x={0}
             y={600}
             width={width}
             fillOpacity={"0"}
             strokeWidth={2}
            />
          </svg>
         {envelope.points.map((point, index) =>
          <Point className={styles.Envelope}
            key={point.id}
            handleClick={() => this.handleClick(envelope, point.id)}
            handleDrag={(e, ui) => this.handleDrag(e, ui, index, envelope)}
            height={height}
            width={width}
            {...point}
            bounds={{x1 : envelope.points[0].position.x, x2: envelope.points[envelope.points.length -1].position.x}}
            active={(point.active || envelope.active) ? true : false}
          />
          )}
         </div>
        )}
        </div>
      )}
    </div>
    );
  }
}
/*Envelope.propTypes = {
  points: PropTypes.arrayOf(
    PropTypes.shape({
      id: PropTypes.number.isRequired,
      position: PropTypes.shape({
              x: PropTypes.number.isRequired,
              y: PropTypes.number.isRequired,
            }).isRequired,
      anchor: PropTypes.bool.isRequired,
      color: PropTypes.string.isRequired,
    }).isRequired,
  ).isRequired,
  height: PropTypes.number.isRequired,
  width: PropTypes.number.isRequired,
  id: PropTypes.number.isRequired,
  active: PropTypes.bool.isRequired,
}*/

const mapStateToProps = (state, ownProps) => {
  var envelopes, minValue, maxValue;
  state.transferfunctions.map(transferfunction => {
          if(transferfunction.id === ownProps.activeVolume){
            envelopes = transferfunction.data.TransferFunction.envelopes;
            minValue = transferfunction.data.MinValue.value;
            maxValue = transferfunction.data.MaxValue.value;
          }
        })
    return {
      envelopes,
      minValue,
      maxValue
    };
};

const mapDispatchToProps = (dispatch, ownProps) => {
  return {
    ToggleEnvelope: (id) => {
      dispatch(toggleActiveEnvelope(id, ownProps.activeVolume));
    },
    TogglePoint: (envelopeId, pointId) => {
      dispatch(toggleActivePoint(envelopeId, pointId, ownProps.activeVolume));
    },
    MovePoint: (position, id, envelopeId) => {
      dispatch(movePoint(id, envelopeId, position, ownProps.activeVolume));
    },
    SwapPoints: ( id, swapId, envelopeId) => {
      dispatch(swapPoints(id, swapId, envelopeId, ownProps.activeVolume));
    },
  }
}

Envelope = connect(
  mapStateToProps,
  mapDispatchToProps,
  )(Envelope)

export default Envelope;
